open Base
open SSA
open UntypedFindDefs


let rec eval_block liveSet = function 
  | stmt::stmts ->
      let rest, restChanged = eval_block liveSet stmts in 
      (match eval_stmt liveSet stmt with 
        | None,_ -> rest, true
        | Some stmt', currChanged -> stmt' :: rest, restChanged || currChanged
       )
  | [] -> [], false 
     
and eval_stmt liveSet stmtNode =  
  match stmtNode.stmt with 
  | Set (ids, rhs) -> 
    if List.exists (fun id -> PSet.mem id liveSet) ids then
      let rhs', changed = eval_exp liveSet rhs in 
      Some {stmtNode with stmt = Set(ids, rhs')}, changed
    else None, true  
  | Ignore rhs -> 
    let rhs', changed = eval_exp liveSet rhs in 
    Some {stmtNode with stmt = Ignore rhs'}, changed
  | SetIdx(id, indices, rhsVal) -> 
     let indexVals, indexChanged = 
       List.split ( List.map (eval_value liveSet) indices)
     in  
     let rhsVal', rhsChanged = eval_value liveSet rhsVal in
     let anyChanged = List.fold_left (||) false indexChanged || rhsChanged in  
     Some {stmtNode with stmt = SetIdx(id, indexVals, rhsVal')}, anyChanged 
  | If (cond, trueBlock, falseBlock, ifGate) ->
    let cond', condChanged = eval_value liveSet cond in
    let trueBlock', trueChanged =  eval_block liveSet trueBlock in 
    let falseBlock', falseChanged = eval_block liveSet falseBlock in 
    if trueBlock' = [] && falseBlock' = [] then None, true 
    else 
      let rec filter_gate_ids outIds falseIds trueIds =
        match outIds, falseIds, trueIds with 
        | [],_,_
        | _, [],_
        | _,_,[] -> [],[],[],false
        | id::ids, t::ts, f::fs ->
            let restIds, restTrue, restFalse, changed = 
              filter_gate_ids ids ts fs 
            in
            if PSet.mem id liveSet then 
              (id::restIds, t::restTrue, f::restFalse, changed)
            else restIds, restTrue, restFalse, true 
      in 
      let outIds, falseIds, trueIds, gateChanged =
        filter_gate_ids ifGate.if_output_ids ifGate.true_ids ifGate.false_ids
      in
      let gate' = {
        if_output_ids = outIds; 
        false_ids=falseIds; 
        true_ids = trueIds
      } in
      let anyChanged = 
        condChanged || trueChanged || falseChanged || gateChanged
      in
      let stmt' =  If(cond', trueBlock', falseBlock', gate') in    
      Some {stmtNode with stmt = stmt'}, anyChanged   

and eval_exp liveSet expNode = match expNode.exp with 
  | Values vs -> 
    let vs', changed = eval_value_list liveSet vs in
    {expNode with exp = Values vs'}, changed  
  | _ -> expNode, false 

and eval_value liveSet vNode =  
 let value',changed=  match vNode.value with 
  | Lam fundef -> 
      let body', changed =  eval_block liveSet fundef.body in 
      Lam {fundef with body = body'}, changed
 | v -> v, false   
 in {vNode with value = value'}, changed 
and eval_value_list liveSet = function 
  | [] -> [], false
  | v::vs -> 
      let v', currChanged = eval_value liveSet v in
      let vs', restChanged = eval_value_list liveSet vs in 
      v'::vs', currChanged || restChanged 
         
(* if we're doing a global analysis we have to identify the top level bindings
   and prevent them from being removed
*) 
let global_elim fnTable block =
  let liveSet, _ = UntypedFindUseSets.find_use_sets block in 
  let topSet = UntypedFindUseSets.find_top_bindings block in 
  eval_block (PSet.union liveSet topSet) block 