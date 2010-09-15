(* map each identifier to the set of identifiers it uses, and also
  collects a liveSet of live identifiers  
*) 

open Base
open SSA

let rec eval_block liveSet = function 
  | [] -> [], false
  | stmt::rest -> 
      let stmts, changed = eval_stmt liveSet stmt in
      let rest, changedRest = eval_block liveSet rest  in  
      stmts @ rest, changed || changedRest  
and eval_stmt liveSet stmtNode =
  let nochange = [stmtNode], false in 
  match stmtNode.stmt with    
  | Set (ids, exp) -> 
      if List.exists (fun id -> PSet.mem id liveSet) ids then 
        let exp', changed = eval_exp liveSet exp in
        let stmtNode' = {stmtNode with stmt= Set(ids, exp')} in  
        [stmtNode'], changed
      else [], true 
  | If (cond, tBlock, fBlock, ifGate) -> nochange
      
and eval_exp liveSet expNode = 
  let nochange = expNode, false   in 
  match expNode.exp with 
  | Values vs -> 
      let vs', changed = eval_value_list liveSet vs in 
      {expNode with exp = Values vs'}, changed 
  | _ -> nochange     
    
and eval_value liveSet vNode = 
  let nochange = vNode, false in
  match vNode.value with  
  | Lam fundef -> 
      let fundef', change = eval_fundef liveSet fundef in 
      {vNode with value = Lam fundef'}, change 
  | _ -> nochange 
and eval_value_list liveSet = function 
  | [] -> [], false
  | v::vs -> 
      let v', currChanged  = eval_value liveSet v in
      let vs', restChanged = eval_value_list liveSet vs in 
      v'::vs', currChanged || restChanged  
and eval_fundef liveSet fundef = 
  let liveSetFn = PSet.of_list (fundef.input_ids @ fundef.output_ids) in
  let body', changed = eval_block (PSet.union liveSet liveSetFn) fundef.body in 
  {fundef with body = body' }, changed   

let elim_dead_code fundef = 
  let liveSet = TypedFindLiveIds.find_live_ids fundef in 
  eval_fundef liveSet fundef 

    