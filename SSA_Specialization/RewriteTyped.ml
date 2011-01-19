open Base
open SSA 
open SSA_Transform
open Printf 
open DynType 


module type REWRITE_PARAMS = sig
  val specializer : value -> Signature.t -> fundef   
  val closureEnv : CollectPartialApps.closure_env
  val tenv : (ID.t, DynType.t) Hashtbl.t 
end  
 
module Rewrite_Rules (P: REWRITE_PARAMS) = struct 
  let dir = Forward
  type context = unit
  let init _ = ()
 (* convert the types hashtbl to a ID.Map.t, store it as the function's 
    type environment 
  *)  
  let finalize _ f = 
    Update {f with 
      tenv = Hashtbl.fold ID.Map.add P.tenv ID.Map.empty;
      fn_input_types = 
        List.map (Hashtbl.find P.tenv) f.input_ids;
      fn_output_types = 
        List.map (Hashtbl.find P.tenv) f.output_ids;   
    } 

  let get_type id = Hashtbl.find P.tenv id 
  let set_type id t = Hashtbl.replace P.tenv id t 
  
  let is_closure id = Hashtbl.mem P.closureEnv.CollectPartialApps.closures id
  
  let get_closure_val id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closures id
    
  let get_closure_args id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closure_args id
    
  let get_closure_arity id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closure_arity id   
  
  let infer_value_type = function 
    | Var id -> get_type id 
    | Num n -> PQNum.type_of_num n 
    | Str _ -> DynType.StrT
    | Sym _ -> DynType.SymT
    | Unit -> DynType.UnitT
    | other -> 
      failwith $ "unexpected SSA value: %s" ^ (SSA.value_to_str other)
  
  let infer_value_node_type valNode = infer_value_type valNode.value   
               
 
  
  (* keeps only the portion of the second list which is longer than the first *) 
  let rec keep_tail l1 l2 = 
    if l1 = [] then l2 
    else if l2 = [] then [] 
    else keep_tail (List.tl l1) (List.tl l2) 
     
  let mk_typed_closure fnVal signature = match fnVal with 
    | Var id ->
      let closureArgs = get_closure_args id in  
      let closureArgTypes = List.map infer_value_node_type closureArgs in 
      let signature' = 
        Signature.prepend_input_types closureArgTypes signature 
      in
      let fnVal' = get_closure_val id in
      let fundef = P.specializer fnVal' signature' in
      {    
        closure_fn = fundef.fn_id;  
        closure_args = closureArgs;  
        closure_arg_types = closureArgTypes;  
        closure_input_types = 
          keep_tail closureArgTypes fundef.fn_input_types; 
        closure_output_types = fundef.fn_output_types;
      }
    | GlobalFn _
    | Prim _ -> 
      let fundef = P.specializer fnVal signature in 
      { 
        closure_fn = fundef.fn_id;  
        closure_args = [];  
        closure_arg_types = [];  
        closure_input_types = fundef.fn_input_types; 
        closure_output_types = fundef.fn_output_types;   
      }
    | _ -> assert false  
                                                                                         
  let annotate_value valNode = 
    let t = infer_value_node_type valNode in  
    if t <> valNode.value_type then Update { valNode with value_type = t} 
    else NoChange  

  let coerce_value t valNode =
    if valNode.value_type = t then NoChange 
    else match valNode.value with 
      | Num n ->
        let n' = PQNum.coerce_num n t in 
        Update (SSA.mk_num ?src:valNode.value_src ~ty:t n')
      | Var id ->   
        let t' = get_type id in
        if t = t' then Update {valNode with value_type = t }
        else  
        let coerceExp = SSA.mk_cast t valNode in    
        let id' =  ID.gen() in 
        set_type id' t;  
        UpdateWithStmts(SSA.mk_var ~ty:t id', [SSA.mk_set [id'] coerceExp])
      | Str _ -> 
        if t = DynType.StrT then 
          Update {valNode with value_type = DynType.StrT}
        else assert false
      | Sym _ -> 
        if t = DynType.SymT then 
          Update {valNode with value_type = DynType.SymT}
        else assert false 
      | Unit ->
        if t = DynType.UnitT then 
          Update{valNode with value_type = DynType.UnitT}
        else assert false    
      | _ ->   
        failwith  $ Printf.sprintf "Can't coerce value %s to type %s"
          (SSA.value_node_to_str valNode) (DynType.to_str t)
  
  
  let rewrite_app fnVal argNodes : exp_node = 
    let argTypes = List.map (fun v -> v.value_type) argNodes in 
    match fnVal with
    | Prim ((Prim.ScalarOp op) as p) -> 
      let outT = TypeInfer.infer_scalar_op op argTypes in
      let commonT = DynType.fold_type_list argTypes in 
      if DynType.is_scalar outT then  
        SSA.mk_primapp p commonT [outT] argNodes
      else 
        let eltTypes = List.map DynType.peel_vec argTypes in 
        let eltSignature = Signature.from_input_types eltTypes in 
        
        let primFundef = P.specializer fnVal eltSignature in
        let primClosure = SSA.mk_closure primFundef [] in 
        let mapNode = SSA.mk_map primClosure argNodes in (
          Printf.printf "fundef: %s\n closure:%s\n mapNode: %s\n"
            (SSA.fundef_to_str primFundef)
            (SSA.closure_to_str primClosure)
            (SSA.exp_to_str mapNode)  
          ; 
          mapNode
        )      
    | Prim ((Prim.ArrayOp op) as p) -> 
   
        let outT = TypeInfer.infer_simple_array_op op argTypes in 
        let commonT = DynType.fold_type_list argTypes in 
        SSA.mk_primapp p commonT [outT] argNodes
    | Prim (Prim.Adverb op) -> failwith "hof not implemented"   
    | GlobalFn _ -> 
      let typedFundef = 
        P.specializer fnVal (Signature.from_input_types argTypes) 
      in
      SSA.mk_call typedFundef.fn_id typedFundef.fn_output_types argNodes   
    | Var id -> 
      if is_closure id then
        let fnVal = get_closure_val id in 
        let closureArgs = get_closure_args id in 
        let closureArgTypes = List.map infer_value_node_type closureArgs in 
        let directArgTypes = 
          List.map (fun vNode -> vNode.value_type) argNodes 
        in 
        let types = closureArgTypes @ directArgTypes in
        let fundef = P.specializer fnVal (Signature.from_input_types types) in 
        SSA.mk_call fundef.fn_id fundef.fn_output_types (closureArgs @ argNodes)   
      else failwith "array indexing"
      
      
  let rewrite_exp 
        (processVal 
          : (value_node -> value_node update) -> value_node -> value_node) 
        types 
        expNode =
    let annotate_values = List.map (processVal annotate_value) in 
    match expNode.exp, types with 
      | Arr elts, [DynType.VecT eltT] ->
        let elts' = 
          List.map (fun elt -> processVal (coerce_value eltT) elt) elts
        in  
        Update {expNode with exp = Arr elts'; exp_types = types }    
      | Values vs, _ ->
        let vs' = 
          List.map2 (fun v t -> processVal (coerce_value t) v) vs types
        in 
        Update {expNode with exp = Values vs'; exp_types = types } 
      | App (fn, args), _ -> 
        Update (rewrite_app fn.value (annotate_values args))       
    
  let rec stmt context helpers stmtNode =
    match stmtNode.stmt with
    | Set(ids, rhs) -> 
        let rhsTypes = List.map (Hashtbl.find P.tenv) ids in
        let rhs' = 
          helpers.process_exp (rewrite_exp helpers.process_value rhsTypes) rhs 
        in 
        Update {stmtNode with stmt = Set(ids, rhs')}
    | SetIdx (arrayId, indices, rhs) -> failwith "setidx not implemented"
    | If(cond, tBlock, fBlock, gate) -> 
        let cond' = helpers.process_value (coerce_value DynType.BoolT) cond in
        let tBlock' = helpers.process_block (stmt context) tBlock in 
        let fBlock' = helpers.process_block (stmt context) fBlock in 
        Update {stmtNode with stmt = If(cond', tBlock', fBlock',gate)}
    | WhileLoop(condBlock, condVal, body, gate) -> 
        let condBlock' = helpers.process_block (stmt context) condBlock in
        let condVal' = 
          helpers.process_value (coerce_value DynType.BoolT) condVal 
        in  
        let body' = helpers.process_block (stmt context) body in
        Update {stmtNode with stmt=WhileLoop(condBlock', condVal', body', gate)} 
    
end 

let rewrite_typed tenv closureEnv specializer fundef =
  let module Params = struct
    let tenv = tenv
    let closureEnv = closureEnv 
    let specializer = specializer 
  end
  in    
  let module Transform = SSA_Transform.MkCustomTransform(Rewrite_Rules(Params))
  in 
  IFDEF DEBUG THEN
    Printf.printf "Rewrite Typed...\n%!";
  ENDIF;
  let fundef, _ = Transform.transform_fundef fundef in fundef   