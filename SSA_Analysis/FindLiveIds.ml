(* collects the set of IDs which are used inside of a function body *) 

open Base
open SSA
open SSA_Analysis 

module Env : HAS_DEFAULT = struct
  type t = ID.t MutableSet.t
  let mk_default () = MutableSet.create 127 
end 
 
module LiveIdAnalysis = struct 
  include MkDefaultAnalysis(Env)(UnitLattice)(UnitLattice)
  
  let init fundef = 
    let liveSet = MutableSet.create 127  in 
    List.iter (MutableSet.add liveSet) fundef.input_ids;   
    List.iter (MutableSet.add liveSet) fundef.output_ids;
    liveSet 

  let var liveSet id = MutableSet.add liveSet id; None
end
  
let find_live_ids = MkEvaluator(LiveIdAnalysis).eval_fundef