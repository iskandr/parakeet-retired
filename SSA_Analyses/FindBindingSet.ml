open Base 
open SSA 
open SSA_Analysis 
module Env  = struct
  type t = ID.Set.t 
  let init fundef = ID.Set.of_list (fundef.input_ids @ fundef.output_ids)  
end
module BindingSetAnalysis = struct
  include SSA_Analysis.MkAnalysis(Env)(ExpUnit)(ValUnit)
  
  let stmt_set env stmtNode ~ids ~rhs ~rhsInfo = Some (ID.Set.add_list ids env)
     
end
module BindingSetEval = SSA_Analysis.MkEvaluator(BindingSetAnalysis)

let fundef_bindings fundef = BindingSetEval.eval_fundef fundef 
let block_bindings block = BindingSetEval.eval_block ID.Set.empty block    