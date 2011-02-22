(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Codegen 
open DynType
open Printf 

(* make a fresh function definition whose body is only an untyped prim *) 
let untypedPrimFnCache : (Prim.prim * int, fundef) Hashtbl.t = 
  Hashtbl.create 127  

let mk_untyped_prim_fundef prim arity : fundef =
  let key = (prim,arity) in 
  if Hashtbl.mem untypedPrimFnCache key  then 
    Hashtbl.find untypedPrimFnCache key
  else 
  let inputs = ID.gen_fresh_list arity in 
  let output = ID.gen() in 
  let bottoms = List.map (fun _ -> DynType.BottomT) inputs in 
  let inputVars = List.map (fun id -> SSA.mk_var id) inputs in 
  let rhs = SSA.mk_app ~types:bottoms (SSA.mk_val (Prim prim)) inputVars in    
  let body = Block.singleton (SSA.mk_set [output] rhs) in 
  let fundef = SSA.mk_fundef inputs [output] body in 
  (Hashtbl.add untypedPrimFnCache key fundef; fundef) 

let mk_typed_scalar_prim (op : Prim.scalar_op) ?optOutType argTypes =   
  let reqArgTypes = TypeInfer.required_scalar_op_types op argTypes in 
  let inferredOutType = TypeInfer.infer_scalar_op op reqArgTypes in 
  let outType = match optOutType with 
    | Some t -> t 
    | None -> inferredOutType 
  in 
  SSA_Codegen.mk_codegen_fn argTypes [outType] $ fun codegen inputs outputs -> 
    let args = Array.of_list inputs in 
    let inTyArr = Array.of_list argTypes in 
    let reqTyArr = Array.of_list reqArgTypes in 
    for i = 0 to Array.length args - 1 do 
      let reqT = reqTyArr.(i) in 
      if inTyArr.(i) <> reqT then begin 
        let id = codegen#fresh_var reqT in   
        codegen#emit [mk_set [id]  (mk_cast reqT args.(i))];
        args.(i) <- codegen#id_value_node id 
      end
    done
    ;
    let primAppNode = 
      SSA.mk_primapp   
        (Prim.ScalarOp op) 
        [outType] 
        (Array.to_list args) 
    in  
    let outputVar = List.hd outputs in 
    codegen#emit [[outputVar] := primAppNode]

let mk_typed_map_fundef ?src nestedFundef inputTypes = 
  let nestedOutputTypes = nestedFundef.SSA.fn_output_types in 
  let outTypes = List.map (fun t -> DynType.VecT t) nestedOutputTypes in
  let closure = SSA.mk_closure nestedFundef [] in 
  SSA_Codegen.mk_codegen_fn inputTypes outTypes $ fun codegen inputs outputs ->
    codegen#emit [outputs := (SSA.mk_map ?src closure inputs )]  

(* checks whether a statement uses an untyped scalar operator *) 
let rec is_scalar_stmt stmtNode = match stmtNode.stmt with 
  | SSA.Set(_, {exp=SSA.App({value=SSA.Prim (Prim.ScalarOp _)}, _)})
  | SSA.Set(_, {exp=Values _}) -> true 
  | SSA.If(_, tCode, fCode, _) -> 
      is_scalar_block tCode && is_scalar_block fCode
  | _ -> false 
and is_scalar_block block = Block.for_all is_scalar_stmt block
  

let rec output_arity interpState closures = function 
  | Var id -> 
      let fnVal = Hashtbl.find closures id in 
      output_arity interpState closures fnVal 
  | GlobalFn fnId -> 
      let fundef = 
        if InterpState.is_untyped_function interpState fnId then
          InterpState.get_untyped_function interpState fnId  
        else 
          InterpState.get_typed_function interpState fnId 
      in  
      List.length fundef.fn_output_types
  | Prim p -> 1
  | _ -> assert false 

    
let rec specialize_fundef interpState fundef signature = 
  IFDEF DEBUG THEN
    Printf.printf "Specialize_Fundef...\n%!";
  ENDIF;
  (* first check to see whether we're mapping a function of scalar operators*)
  (* over vector data. if so, rather than creating a large number of Map nodes *)
  (* and then merging them we directly create a single Map *) 
  let inTypes = Signature.input_types signature in 
  if not (Signature.has_output_types signature) && 
     List.exists DynType.is_vec inTypes &&  
     is_scalar_block fundef.body 
  then scalarize_fundef interpState fundef signature 
  else 
  let fundef', closureEnv = 
    CollectPartialApps.collect_partial_apps interpState fundef 
  in
  
  let outputArity : SSA.value -> int = 
    output_arity interpState closureEnv.CollectPartialApps.closures
  in
  let specializer = specialize_value interpState in  
  (* to avoid having to make TypeAnalysis and Specialize recursive 
       modules I've untied the recursion by making specialize_value 
       a parameter of TypeAnalysis. 
   *)  
  let tenv = 
    TypeAnalysis.type_analysis 
      ~specializer 
      ~output_arity:outputArity 
      ~closureEnv 
      ~fundef:fundef' 
      ~signature 
  in
  let typedFn = 
    RewriteTyped.rewrite_typed 
      ~tenv 
      ~closureEnv 
      ~specializer 
      ~output_arity:outputArity 
      ~fundef:fundef' 
  in
  typedFn
     
and scalarize_fundef interpState untypedFundef vecSig =
  let inTypes = Signature.input_types vecSig in 
  let scalarSig = 
    Signature.from_input_types (List.map DynType.peel_vec inTypes) 
  in 
  let scalarFundef = 
    specialize_value interpState (SSA.GlobalFn untypedFundef.fn_id) scalarSig
  in 
  let scalarOutputTypes = scalarFundef.fn_output_types in   
  let outTypes = List.map (fun t -> VecT t) scalarOutputTypes in  
  let scalarClosure = SSA.mk_closure scalarFundef [] in
  SSA_Codegen.mk_codegen_fn inTypes outTypes (fun codegen inputs outputs ->
    let outIds = List.map SSA.get_id outputs in  
    codegen#emit [ 
      SSA.mk_set outIds (SSA.mk_map scalarClosure inputs)
    ]    
  ) 
 
and specialize_value interpState fnVal signature =
  (*IFDEF DEBUG THEN
    Printf.printf "Specialize_Value %s :: %s\n%!"
      (SSA.value_to_str fnVal)
      (Signature.to_str signature)
    ; 
  ENDIF;
  *)
  match InterpState.maybe_get_specialization interpState fnVal signature with
  | Some fnId -> InterpState.get_typed_function interpState fnId
  | None ->  
    let inputTypes = Signature.input_types signature in  
    (match fnVal with 
      | SSA.GlobalFn fnId -> 
        let fundef = InterpState.get_untyped_function interpState fnId in 
        let typedFundef = specialize_fundef interpState fundef signature in 
          InterpState.add_specialization 
            ~optimize:true 
            interpState 
            fnVal 
            signature 
            typedFundef;
          typedFundef
      | SSA.Prim (Prim.ScalarOp op) ->
          let optOutType = 
            Option.map List.hd (Signature.output_types_option signature)
          in
          let typedFundef = 
            if List.for_all DynType.is_scalar inputTypes then 
             mk_typed_scalar_prim op ?optOutType inputTypes 
          else
            let nestedInputTypes = List.map DynType.peel_vec inputTypes in 
            let nestedSig = 
              match optOutType with 
                | None -> Signature.from_input_types nestedInputTypes
                | Some outT -> 
                  Signature.from_types nestedInputTypes [DynType.peel_vec outT]
            in
            let nestedFundef = specialize_value interpState fnVal nestedSig in
            mk_typed_map_fundef nestedFundef inputTypes
          in   
          InterpState.add_specialization
            ~optimize:false 
            interpState 
            fnVal
            signature 
            typedFundef
          ;
          typedFundef    

      | SSA.Prim p -> 
          let arity = List.length inputTypes in
          assert (arity >= Prim.min_prim_arity p && 
                  arity <= Prim.max_prim_arity p); 
          let fundef = mk_untyped_prim_fundef p arity in 
          print_string $ "A: " ^ (SSA.fundef_to_str fundef) ;
          let typedFundef = specialize_fundef interpState fundef signature in
          print_string "B\n"; 
          InterpState.add_specialization 
            ~optimize:false 
            interpState 
            fnVal 
            signature 
            typedFundef;
          print_string "C\n";
          typedFundef
      | _ -> assert false 
    )
    
and specialize_function_id interpState fnId signature = 
  IFDEF DEBUG THEN
    Printf.printf "Specialize_Function_Id...\n%!";
  ENDIF;
  specialize_value interpState (GlobalFn fnId) signature
    