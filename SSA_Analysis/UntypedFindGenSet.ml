open Base 
open SSA 

module Env : SSA_Analysis.SIMPLE_ANALYSIS = struct
  type env = ID.Set.t 
  let init fundef = ID.Set.empty 
end
module GenSetAnalysis : SSA_Analysis.ANALYSIS = struct
  include SSA_Analysis.MakeSimpleAnalysis(Env) 
  let set env ids _ _ = Update (ID.Set.add_list ids env) 
end
module GenSetEval = SSA_Analysis.MakeEvaluator(GenSetAnalysis)

let block_gens block = GenSetEval.eval_block ID.Set.empty block    
