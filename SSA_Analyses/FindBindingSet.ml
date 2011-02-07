open Base 
open SSA 
open SSA_Analysis 

module Env  = struct
  type env = ID.Set.t
  type value = unit 
  
  let init fundef = ID.Set.of_list (fundef.input_ids @ fundef.output_ids)
  let add set id _ = ID.Set.add id set   
  let mem set id = ID.Set.mem id set 
  let find _ _ = assert false  
end
module BindingSetAnalysis = struct
  include SSA_Analysis.MkAnalysis(Env)(UnitLattice)
  
  let stmt_set env stmtNode ~ids ~rhs ~rhsInfo = Some (ID.Set.add_list ids env)
     
end
module BindingSetEval = SSA_Analysis.MkEvaluator(BindingSetAnalysis)

let fundef_bindings fundef = BindingSetEval.eval_fundef fundef 
let block_bindings block = BindingSetEval.eval_block ID.Set.empty block    