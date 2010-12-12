open Base
open SSA
open SSA_Analysis 

module Env = struct 
  type t = (ID.t, int) Hashtbl.t
  let init _ = Hashtbl.create 127 
end

module UseCountAnalysis = struct
  include MkSimpleAnalysis(Env) 
  
  let var (counts : env) (id : ID.t) : value_info = 
    let oldCount = Hashtbl.find_default counts id 0 in 
    Hashtbl.add counts id (oldCount+1)
end 

module UseCountEval = MkEvaluator(UseCountAnalysis)
let find_fundef_use_counts = UseCountEval.eval_fundef