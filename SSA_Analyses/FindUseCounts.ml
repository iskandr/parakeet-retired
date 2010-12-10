open Base
open SSA
open SSA_Analysis 

module Env : HAS_DEFAULT = struct 
  type t = (ID.t, int) Hashtbl.t
  let mk_default () = Hashtbl.create 127 
end

module UseCountAnalysis : ANALYSIS = struct
  include MkDefaultAnalysis(Env)(UnitLattice)(UnitLattice)
  
  let var counts id =
    let oldCount = Hashtbl.find_default counts id 0 in 
    Hashtbl.add counts id (oldCount+1); 
    None 
end 

module EvalUseCounts = SSA_Analysis.MkEvaluator(UseCountAnalysis)

let find_fundef_use_counts = EvalUseCounts.eval_fundef