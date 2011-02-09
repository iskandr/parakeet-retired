open Base 
open SSA 
open SSA_Analysis 

module BindingSetEval = SSA_Analysis.MkEvaluator(struct 
  type env = ID.Set.t
  type value_info = unit
  type exp_info = unit 
  
  let dir = Forward
  let clone_env = env 
  let iterative = false 
  
  let init fundef = ID.Set.of_list (fundef.input_ids @ fundef.output_ids)
  
  let value _ _ = ()
  let exp _ _ _ = ()
  let stmt env stmtNode helpers = match stmtNode.stmt with 
    | Set(ids, _) -> Some (ID.Set.add_list ids env)
    | _ -> helpers.visit_stmt env stmtNode  
     
end)

let fundef_bindings fundef = BindingSetEval.eval_fundef fundef 
let block_bindings block = BindingSetEval.eval_block ID.Set.empty block    