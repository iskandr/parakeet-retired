(* collects the set of IDs which are used inside of a function body *) 
open Base
open SSA
open SSA_Analysis 

module Env = struct
  type t = ID.t MutableSet.t 
  let init fundef = 
    let liveSet = MutableSet.create 127  in 
    List.iter (MutableSet.add liveSet) fundef.input_ids;   
    List.iter (MutableSet.add liveSet) fundef.output_ids;
    liveSet   
end
module LiveIdAnalysis = struct
  include MkAnalysis(Env)(ExpUnit)(ValUnit)
      
  let value liveSet valNode = match valNode.value with 
    | Var id -> MutableSet.add liveSet id
    | _ -> () 
end
module LiveIdEval = MkEvaluator(LiveIdAnalysis) 
let find_live_ids = LiveIdEval.eval_fundef 