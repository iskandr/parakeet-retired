(* pp: -parser o pa_macro.cmo *)

open Base
open SSA 
open SSA_Transform 

let rec replace_id_list = function 
  | [] -> [], false
  | id::ids -> 
    let ids', changed = replace_id_list ids in 
    if ID.Map.mem id idMap then (ID.Map.find id idMap)::ids', true
    else id::ids', changed 
  
let replace_value idMap valNode = match valNode.value with  
  | Var id -> 
    if ID.Map.mem id idMap then 
      { valNode with value= Var (ID.Map.find id idMap) }
    else valNode  
  | _ -> valNode 

let replace_values idMap valNodes = match valNodes with  
  | [] -> []
  | v::vs -> 
    let v' = replace_value idMap v in 
    let vs' = replace_values idMap vs in 
    if v ==  v' && vs == vs' then valNodes 
    else v'::vs'  

let replace_exp idMap expNode = match expNode.exp with 
  | App (fn,args) ->
    let fn' = replace_value idMap fn in 
    let args' = replace_values idMap args in 
    if fn != fn' || args != args' then {expNode with exp = App(fn', args')}
    else expNode 
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *) 
  | Cast of DynType.t * value_node  
  | Call of typed_fn * value_nodes 
  | PrimApp of typed_prim * value_nodes  
  | Map of closure * value_nodes
  | Reduce of closure * closure * value_nodes   
  | Scan of closure * closure * value_nodes 
  | Arr elts ->
  | Values vals ->
  | Cast (t, v) -> of DynType.t * value_node  
  | Call (typedFn, args) ->  
  | PrimApp (typedPrim, args) ->   
  | Map (closure, args) -> 
  | Reduce (initClosure, reduceClosure, args) ->    
  | Scan (initClosure, reduceClosure, args) -> 
     
let stmt cxt helpers stmtNode = match stmtNode.stmt with 
    | Set (ids, rhs) ->
        let ids', changed = replace_id_list ids in 
        if changed then 
          Update (SSA.mk_set ?src:stmtNode.stmt_src ids' rhs)
        else NoChange 
    | SetIdx (id, idxs, rhs) ->
        if ID.Map. mem id idMap then 
          let id' = ID.Map.find id idMap in 
          Update (SSA.mk_stmt ?src:stmtNode.stmt_src $ SetIdx(id', idxs, rhs))
        else NoChange  
    | If (c, t, f, gate) ->
      let outIds, outChanged  = replace_id_list gate.if_output_ids in
      let trueIds, trueChanged = replace_id_list gate.true_ids in
      let falseIds, falseChanged = replace_id_list gate.false_ids in
      if outChanged || trueChanged || falseChanged then 
        let gate' = { 
          if_output_ids = outIds; true_ids = trueIds; false_ids = falseIds
        } in Update (SSA.mk_if ?src:stmtNode.stmt_src c t f gate') 
      else NoChange   
    | _ -> NoChange 
      

   
end

      
let replace_block idMap block =  
  let module Params = struct let idMap = idMap end in 
  let module Replacer = MkSimpleTransform(Replace_Rules(Params)) in  
  Replacer.transform_block () block  
     
let replace_fundef idMap fundef = 
  let body', changed = replace_block idMap fundef.body in 
  {fundef with body = body'}, changed   