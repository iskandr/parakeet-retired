(* map each identifier to the set of identifiers it uses, and also
  collects a liveSet of live identifiers  
*) 

open Base
open SSA

let rec eval_block liveSet = function 
  | [] -> liveSet
  | stmt::stmts -> 
      let liveSet' = eval_stmt liveSet stmt in 
      eval_block liveSet' stmts 
and eval_stmt liveSet stmtNode = match stmtNode.stmt with    
  | Set (_, exp) -> eval_exp liveSet exp 
  | If (cond, tBlock, fBlock, ifGate) ->
      let liveSet1 = eval_value liveSet cond in 
      let liveSet2 = eval_block liveSet1 tBlock in 
      eval_block liveSet2 fBlock  
  | SetIdx(id, indices, rhs) -> 
      let liveSet1 = eval_value_list liveSet indices in
      let liveSet2 = eval_value liveSet1 rhs in
      (* does a variable become live just because we're
         modifying it? 
      *)
      ID.Set.add id liveSet2  
  | WhileLoop _ -> failwith "[FindLiveIds] loops not yet implemented"
and eval_exp liveSet (expNode:exp_node) = match expNode.exp with  
  | ArrayIndex (lhs, args)
  | App (lhs, args) -> eval_value_list liveSet (lhs::args) 
  | Cast (_, rhs) -> eval_value liveSet rhs  
  | Arr vs -> eval_value_list liveSet vs
  | Values vs -> eval_value_list liveSet vs  
and eval_value liveSet ( valNode: value_node) = match valNode.value with 
  | Lam fundef -> eval_block liveSet fundef.body 
  | Var id -> ID.Set.add id liveSet  
  | GlobalFn _ 
  | Str _
  | Sym _
  | Unit _
  | Prim _
  | Num _  -> liveSet 
and eval_value_list liveSet = function 
  | [] -> ID.Set.empty 
  | v::vs -> 
      let currSet = eval_value liveSet v in 
      let restSet = eval_value_list liveSet vs in
      ID.Set.union currSet restSet 

let find_live_ids fundef = 
  let liveSet = ID.Set.of_list (fundef.input_ids @ fundef.output_ids) in 
  eval_block liveSet fundef.body 
    