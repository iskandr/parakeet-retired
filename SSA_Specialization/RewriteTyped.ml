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
  val specializer : value -> DynType.t list -> FnId.t  
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

  let infer_value valNode = 
    let t = match valNode.value with 
    | Var id -> Hashtbl.find P.tenv id 
    | Num n -> PQNum.type_of_num n 
    | Str _ -> DynType.StrT
    | Sym _ -> DynType.SymT
    | Unit -> DynType.UnitT
    | _ -> 
      failwith $ "unexpected SSA value: %s" ^ (SSA.value_node_to_str valNode)
    in     
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
  
  
  let rewrite_app tenv fnVal argNodes = match fnVal with 
    | Var id -> () 

          
  let rewrite_exp 
        (processVal : (value_node->value_node update)->value_node->value_node)
        expNode 
        types =
    let process_values = List.map (processVal infer_value) in 
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
        let args' = process_values args in 
        rewrite_app fn.value args'       
    in 
    Update {expNode with exp=exp'; exp_types = types}         
  
  let rewrite_stmt context processExp processVal stmtNode =
    match stmtNode.stmt with
    | Set(ids, rhs) -> 
        let rhsTypes = List.map (Hashtbl.find P.tenv) ids in 
        let rhs', stmts, changed = coerce_exp rhs rhsTypes in 
        if changed then 
          let stmtNode' = {stmtNode with stmt = Set(ids, rhs') } in 
          Some (stmts @ [stmtNode'])
        else None 
    | _ -> None 

end 

let rewrite_typed tenv closureEnv specializer fundef =
  let module Params = struct
    let tenv = tenv
    let closureEnv = closureEnv 
    let specializer = specializer 
  end
  in    
  let module Transform = SSA_Transform.MkTransformation(Rewrite_Rules(Params))
  in
  Transform.rewrite_fundef fundef  