open Base
open SSA

module Env : SSA_Analysis.SIMPLE_ANALYSIS = struct 
  type env = (ID.t, int) Hashtbl.t
  let init fundef = 
    let env = Hashtbl.create 127 in 
    List.iter (fun id -> Hashtbl.add initCounts id 1) fundef.output_ids;
    env  
end
module UseCountAnalysis : SSA_Analysis.ANALYSIS = struct
  include SSA_Analysis.MakeSimpleAnalysis(Env)
  let var counts id =
    let oldCount = Hashtbl.find_default counts id 0 in 
    Hashtbl.add counts id (oldCount+1); SSA_Analysis.Update counts 
end 
module EvalUseCounts = SSA_Analysis.MakeEvaluator(UseCountAnalysis)

let find_fundef_use_counts = EvalUseCounts.eval_fundef