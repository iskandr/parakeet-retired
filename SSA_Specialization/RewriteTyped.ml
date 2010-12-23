open Base
open SSA 
open SSA_Transform
open Printf 
open DynType 

(* Given a type environment and a list of annotated but untyped stmtNodes *)
(* return a list of typed statement nodes and the modified type environment*)
(* which might contain the types produced by any inserted coercions. *)
(* However, don't try to "coerce" functions since function values *)
(* are assumed to have been correctly specialized by the annotator.  *)

module type REWRITE_PARAMS = sig
  val specializer : value -> DynType.t list -> fundef   
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
    Update {f with tenv = Hashtbl.fold ID.Map.add P.tenv ID.Map.empty } 

  let get_type id = Hashtbl.find P.tenv id 
  let set_type id t = Hashtbl.replace P.tenv id t 
  
  let is_closure id = Hashtbl.mem P.closureEnv.CollectPartialApps.closures id
  
  let get_closure_val id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closures id
    
  let get_closure_args id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closure_args id
  
    
  let get_closure_arity id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closure_arity id   
  
  let value_type = function 
    | Var id -> get_type id 
    | Num n -> PQNum.type_of_num n 
    | Str _ -> DynType.StrT
    | Sym _ -> DynType.SymT
    | Unit -> DynType.UnitT
    | other -> 
      failwith $ "unexpected SSA value: %s" ^ (SSA.value_to_str other)
  
  let value_node_type valNode = value_type valNode.value    
  
  (* keeps only the portion of the second list which is longer than the first *) 
  let rec keep_tail l1 l2 = 
    if l1 = [] then l2 
    else if l2 = [] then [] 
    else keep_tail (List.tl l1) (List.tl l2) 
     
  let mk_typed_closure fnVal signature = match fnVal with 
    | Var id ->
      let closureArgs = get_closure_args id in  
      let closureArgTypes = List.map value_node_type closureArgs in 
      let signature' = Signature.prepend_input_types closureArgTypes signature in
      let fnVal' = get_closure_val id in
      let fundef = P.specializer fnVal' signature' in
      {    
        closure_fn = fundef.fundef_id;  
        closure_args = closureArgs;  
        closure_arg_types = closureArgTypes;  
        closure_input_types = 
          keep_tail closureArgTypes fundef.fundef_input_types; 
        closure_output_types = primFundef.fundef_output_types;
      }
    | GlobalFn _
    | Prim _ -> 
      let fundef = P.specializer fnVal signature in 
      { 
        closure_fn = primFundef.fundef_id;  
        closure_args = [];  
        closure_arg_types = [];  
        closure_input_types = primFundef.fundef_input_types; 
        closure_output_types = primFundef.fundef_output_types;   
      }
    | _ -> assert false  
                      
  let infer_value valNode = 
    let t = value_node_type valNode in  
    if t <> valNode.value_type then Update { valNode with value_type = t} 
    else NoChange  

  let coerce_value t valNode =
    if valNode.value_type = t then NoChange 
    else match valNode.value with 
      | Num n ->
        let n' = PQNum.coerce_num n t in 
        Update (SSA.mk_num ~value_src:valNode.value_src ~ty:t n')
      | Var id ->   
        let t' = Hashtbl.find tenv id in
        if t = t' then Update {valNode with value_type = t }
        else  
        let coerceExp = SSA.mk_cast t valNode in    
        let id' =  ID.gen() in 
        add_type id' t;  
        UpdateWithStmts(SSA.mk_var ~ty:t id', [SSA.mk_set [id'] coerceExp])
      | Str _ -> 
        if t = DynType.StrT then 
          Update {valNode with value_type = DynType.StrT}
        else assert false
      | Sym _ -> 
        if t = DynType.SymT then 
          Update {valNode with value_type = DynType.SymT}
        else assert false 
      | Unit, DynType.UnitT ->
        if t = DynType.UnitT then 
          Update{valNode with value_type = DynType.UnitT}
        else assert false    
      | _ ->   
        failwith  $ Printf.sprintf "Can't coerce value %s to type %s"
          (SSA.value_node_to_str valNode) (DynType.to_str t)
  
  
  let rewrite_app fnVal argNodes = 
    let argTypes = List.map (fun v -> v.value_type) argNodes in 
    match fnVal with
    | GlobalFn fnId -> 
      let typedFundef = P.specializer fnVal argTypes in
      SSA.mk_call typedFnId argNodes   
      
    | Prim p ->  
    | Var id -> 
      if is_closure id then
        let closureval = get_closure_val id in 
        let closureArgIds = get_closure_args id in 
        let closureArgTypes = List.map get_type closureArgIds in 
        let directArgTypes = 
          List.map (fun vNode -> vNode.value_type) argNodes 
        in 
        let types = closureArgTypes @ directArgTypes in 
        (* specialize closureVal to types *) 
        
        (* ... *) 
        let closureArgVals = 
          List.map2 
            (fun id t -> SSA.mk_var ~ty:t id) 
            closureArgIds 
            closureArgTypes 
        in 
        ()
      else (* array indexing? *)   
  let rewrite_exp processVal types expNode =
    let infer_values = List.map (processVal infer_value) in 
    let exp' = match expNode.exp, types with 
      | Arr elts, [DynType.VecT eltT] ->
        let elts' =
          List.map (fun elt -> processVal (coerce_value eltT) elt) elts 
        in 
        Arr elts'  
      | Values vs, _ ->
        let vs' = List.map2 (fun v t -> processVal (coerce_value t) vs types in 
        Values vs'
      | App (fn, args) ->
        let args' = infer_values args in 
        rewrite_app fn.value args'       
    in 
    Update {expNode with exp=exp'; exp_types = types}         
  
  let stmt context helpers stmtNode =
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
        Some [{stmtNode with stmt = If(cond', tBlock', fBlock',gate)}]
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
  Transform.rewrite_fundef fundef  