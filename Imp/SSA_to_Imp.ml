(* pp: -parser o pa_macro.cmo *)

open DynType
open Base
open Imp

let get_ssa_shape (sizeEnv : SymbolicShape.shape ID.Map.t) valNode = 
  match valNode.SSA.value with 
  | SSA.Var id -> ID.Map.find id sizeEnv 
  | _ -> SymbolicShape.scalar

let rec translate_value idEnv valNode = 
  let vExp = match valNode.SSA.value with
    | SSA.Var id -> Imp.Var (ID.Map.find id idEnv)
    | SSA.Num n -> Imp.Const n
    | _ -> failwith "[ssa->imp] value not implemented "
  in
  { Imp.exp = vExp; Imp.exp_type = valNode.SSA.value_type }
  
and translate_exp 
      (codegen : ImpCodegen.imp_codegen) 
      (fnTable : FnTable.t) 
      (ssaSizeEnv : SymbolicShape.shape ID.Map.t)
      idEnv 
      expectedType 
      expNode = 
  let impExpNode = match expNode.SSA.exp with
  | SSA.Values [v] -> translate_value idEnv v
  | SSA.Values [] -> failwith "[ssa->imp] unexpected empty value list"
  | SSA.Values _ ->  failwith "[ssa->imp] unexpected multiple return values"
    
  | SSA.PrimApp (Prim.ScalarOp Prim.Select, [cond; tVal; fVal]) ->  
      let cond' = translate_value idEnv cond in 
      let tVal' = translate_value idEnv tVal in 
      let fVal' = translate_value idEnv fVal in 
      select cond' tVal' fVal' 
  | SSA.PrimApp (Prim.ScalarOp op, vs) -> 
      let vs' = List.map (translate_value idEnv) vs in 
      let argT = (List.hd vs').exp_type in  
      if Prim.is_comparison op then cmp_op op ~t:argT vs' 
      else typed_op op vs'
  | SSA.PrimApp (Prim.ArrayOp Prim.Find, [inArray; elToFind]) -> 
    let arrT = inArray.SSA.value_type in 
    let valT = elToFind.SSA.value_type in 
    let inArray' = translate_value idEnv inArray in
    let elToFind' = translate_value idEnv elToFind in
    let i = codegen#fresh_var Int32T in
    let index = codegen#fresh_var Int32T in
    let n = codegen#fresh_var Int32T in
    codegen#emit [
      set i (int 0);
      set n (len inArray');
      set index n;
      while_ ((i <$ n) &&$ (index =$ n)) [
        ifTrue ((idx inArray' i) =$ elToFind') [set index i]
      ]
    ];
    index
  | SSA.PrimApp (Prim.ArrayOp Prim.Where, [_]) -> 
    failwith ("WHERE operator can't be translated to Imp " ^ 
              "due to unpredictable size semantics") 

  
  | SSA.Cast (t, vNode) -> cast t (translate_value idEnv vNode) 
  (* assume you only have one array over which you're mapping for now *)
  | SSA.Map(payload, arrays) ->
      let eltOutTypes = payload.SSA.closure_output_types in
      (* TODO: make this work for multiple outputs *)  
      let eltOutType = List.hd eltOutTypes in
      let vecOutType = DynType.VecT eltOutType in 
      let fnId = payload.SSA.closure_fn in 
      let fundef_ssa = FnTable.find fnId fnTable in
      let fundef_imp = translate_fundef fnTable fundef_ssa in
      
      let arrays_imp = List.map (translate_value idEnv) arrays in
      (* array arg of highest rank *)
      let maxArray_ssa = SymbolicShape.largest_ssa_val arrays in
      let maxArrayShape = get_ssa_shape ssaSizeEnv maxArray_ssa in  
      let output = 
        codegen#fresh_var 
          ~dims:maxArrayShape   
          ~storage:Imp.Private
          vecOutType 
      in  
      let i = codegen#fresh_var Int32T in
      let n = codegen#fresh_var Int32T in
      (* since shapes are imp expressions, we can just take the outermost
         dim of the symbolic shape and put it as the loop boundary *) 
      let loopBoundary = List.hd maxArrayShape in 
      let bodyBlock = [
        set i (int 0);
        set n loopBoundary;
        while_ (i <$ n) [SPLICE; set i (i +$ (int 1))]
      ] in
      let lhs = [|idx output i|] in 
      let rhs = Array.of_list (List.map (fun arr -> idx arr i) arrays_imp) in 
      codegen#splice_emit fundef_imp rhs lhs bodyBlock;
      output
  
  (* assume you only have one initial value, and only one scalar output *)    
  | SSA.Reduce(initClosure, closure, initArgs, arrays) ->
      (* TODO: make use of initClosure *)  
      let accTypes = closure.SSA.closure_output_types in    
      let arrayTypes = List.map (fun v -> v.SSA.value_type) arrays in 
      (* TODO: make this work for multiple outputs *)  
      let outputType = List.hd accTypes in
      let initial = List.hd initArgs in 
      let fundef = FnTable.find closure.SSA.closure_fn  fnTable in 
      let impPayload = translate_fundef fnTable fundef in 
      let impInit = translate_value idEnv initial in
      assert (arrays <> []);  (* assume at least one array *) 
      let impArrays = List.map (translate_value idEnv) arrays in
      (* for now assume acc is a scalar *) 
      let acc = codegen#fresh_var outputType in
      let i = codegen#fresh_var Int32T in
      let n = codegen#fresh_var Int32T in  
      (* alex: fixing a bug wherein the "arrays" are actually scalars *)
      IFDEF DEBUG THEN 
        assert (List.length impArrays = List.length arrayTypes); 
      ENDIF; 
      let arrayElts = 
        List.map2 
          (fun arr t -> if DynType.is_scalar t then arr else idx arr i) 
          impArrays
          arrayTypes   
      in
      let payloadArgs = Array.of_list (acc :: arrayElts) in
      let payloadOutputs = [|acc|] in
      let bodyBlock = 
        if List.exists DynType.is_vec arrayTypes then 
        [
          set i (int 0);
          (* assume arrays are of the same length *) 
          set n (len $ List.hd impArrays);  
          set acc impInit; 
          while_ (i <$ n) [SPLICE; set i (i +$ (int 1))] 
        ]
        (* if all arguments are scalars, just call the function directly *)
        else [SPLICE] 
      in 
      codegen#splice_emit impPayload payloadArgs payloadOutputs bodyBlock;
      acc
    
  | _ -> failwith $ 
    Printf.sprintf 
      "[ssa->imp] typed core exp not yet implemented: %s"
      (SSA.exp_to_str expNode)
  in 
  impExpNode

and translate_stmt 
      (fnTable : FnTable.t)
      (codegen : ImpCodegen.imp_codegen) 
      (ssaSizeEnv : SymbolicShape.shape ID.Map.t)  
      (idEnv : ID.t ID.Map.t)
      (stmtNode : SSA.stmt_node) = match stmtNode.SSA.stmt with
  | SSA.Set([id], expNode) ->
      (match expNode.SSA.exp_types with
        | [t] ->
          let id' = ID.Map.find id idEnv in
          let exp' = translate_exp codegen fnTable ssaSizeEnv idEnv t expNode in
          let varNode = {Imp.exp = Var id'; exp_type = t } in 
          codegen#emit [set varNode exp']
        | _ -> failwith "[ssa->imp] expected only single value on rhs of set"
       )
  | SSA.Set(ids, {SSA.exp=SSA.Values vs;SSA.exp_types=exp_types}) ->
      let rec flatten_assignment ids types vs = match ids, types, vs with 
        | id::restIds, t::restTypes, v::restValues ->
           let impId = ID.Map.find id idEnv in
           let rhs = translate_value idEnv v in
           codegen#emit [set (var ~t impId) rhs];
           flatten_assignment restIds restTypes restValues   
        | [], [], [] -> ()  
        | _ -> failwith "[ssa->imp] length mismatch in set stmt"
      in flatten_assignment ids exp_types vs
  | SSA.Set(_::_::_, _) -> 
      failwith "[ssa->imp] multiple return values not implemented"
  | _ -> failwith "[ssa->imp] stmt not implemented"      

and translate_fundef fnTable fn =
  let codegen  = new ImpCodegen.imp_codegen in
  let inputTypes = fn.SSA.fn_input_types in 
  let outputTypes = fn.SSA.fn_output_types in
  (* first generate imp ids for inputs, to make sure their order is preserved *)
  let add_input env id t = 
    let impId = codegen#fresh_input_id t in 
    ID.Map.add id impId env  
  in 
  let inputIdEnv = 
    List.fold_left2 add_input ID.Map.empty fn.SSA.input_ids inputTypes   
  in 
  (* next add all the live locals, along with their size expressions *)
  let liveIds : ID.t MutableSet.t = FindLiveIds.find_live_ids fn in
  let sizeEnv : SymbolicShape.shape ID.Map.t = 
    ShapeInference.infer_shape_env fnTable fn  
  in
  let add_local id env =
    (* ignore inputs since they were already handled *) 
    if List.mem id fn.SSA.input_ids then env
    else 
    let t = ID.Map.find id fn.SSA.tenv in
    let dims : Imp.exp_node list = ID.Map.find id sizeEnv in
    (* rename all vars to refer to new Imp IDs, instead of old SSA IDs *)
    let dims' : Imp.exp_node list =
       List.map (ImpReplace.apply_id_map env) dims 
    in   
    let impId = 
      (if List.mem id fn.SSA.output_ids then 
        codegen#fresh_output_id ~dims:dims' t 
      else
        codegen#fresh_local_id ~dims:dims' t
      )
    in  
    IFDEF DEBUG THEN 
        Printf.printf "[ssa2imp] Renamed %s to %s\n"
          (ID.to_str id)
          (ID.to_str impId); 
    ENDIF; 
    ID.Map.add id impId env    
  in  
  let idEnv = MutableSet.fold add_local liveIds inputIdEnv in
  Block.iter_forward (translate_stmt fnTable codegen sizeEnv idEnv) fn.SSA.body;
  let impFn =  codegen#finalize in 
  IFDEF DEBUG THEN 
    Printf.printf "[ssa2imp] Translated %s into: %s\n"
      (FnId.to_str fn.SSA.fn_id)
      (Imp.fn_to_str impFn);
  ENDIF;
  impFn 