open Base 
open SSA 

module Env  = struct
  type t = ID.Set.t 
  let init fundef = ID.Set.empty 
end
module GenSetAnalysis : SSA_Analysis.ANALYSIS = struct
  include SSA_Analysis.MakeSimpleAnalysis(Env) 
  let stmt env stmtNode info = match stmtNode.stmt with 
    | Set (ids, _) -> Update (ID.Set.add_list ids env)
    | _ -> failwith "not implemented"  
end
module GenSetEval = SSA_Analysis.MakeEvaluator(GenSetAnalysis)

let block_gens block = GenSetEval.eval_block ID.Set.empty block    