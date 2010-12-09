(* collects the set of IDs which are used inside of a function body *) 

open Base
open SSA

module Env = struct 
  type env = ID.t MutableSet.t
  let init fundef = 
    let liveSet = MutableSet.create 127  in 
    List.iter (MutableSet.add liveSet) fundef.input_ids;   
    List.iter (MutableSet.add liveSet) fundef.output_ids;
    liveSet
end 

module LiveIdAnalysis = struct 
  include SSA_Analysis.MakeSimpleAnalysis(Env)
  let var liveSet id = MutableSet.add liveSet id  
end
  
let find_live_ids = SSA_Analysis.Make(LiveIdAnalysis).analyze_fundef