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
  | Set (ids, ({exp=Values vs} as expNode)) -> 
      let vs', changedVals = eval_value_list liveSet vs in
      let rec filter ids vs ts = match ids, vs, ts with 
        | [], [], [] -> [], [], [], false
        | id::ids, v::vs, t::ts -> 
          let restIds, restVals, restTypes, restChanged = filter ids vs ts in 
          if ID.Set.mem id liveSet then 
             id::restIds, v::restVals, t::restTypes, restChanged
          else restIds, restVals, restTypes, true
        | _ -> failwith "[elim_dead_code] mismatching lengths"
      in
      let filteredIds, filteredVals, filteredTypes, changedIds = 
        filter ids vs' expNode.exp_types 
      in
      if filteredIds= [] then [], true
      else
        let expNode' = 
          {expNode with exp = Values filteredVals; exp_types = filteredTypes } 
        in    
        let stmtNode' = {stmtNode with stmt=Set(filteredIds, expNode')} in  
        [stmtNode'], changedVals || changedIds
        
  | Set (ids, exp) -> 
      if List.exists (fun id -> ID.Set.mem id liveSet) ids then 
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
and eval_fundef (liveSet : ID.Set.t) fundef = 
  let liveSetFn = ID.Set.of_list (fundef.input_ids @ fundef.output_ids) in
  let body', changed = 
    eval_block (ID.Set.union liveSet liveSetFn) fundef.body 
  in 
  {fundef with body = body'}, changed   

let elim_dead_code 
      (fnTable : FnTable.t) 
      (fundef : SSA.fundef) : SSA.fundef * bool = 
  let liveSet : ID.Set.t = FindLiveIds.find_live_ids fundef in 
  eval_fundef liveSet fundef 


    