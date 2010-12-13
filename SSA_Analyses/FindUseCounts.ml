open Base
open SSA
open SSA_Analysis 

module Env = struct 
  type t = (ID.t, int) Hashtbl.t
  let init _ = Hashtbl.create 127 
end

module UseCountAnalysis = struct
  include MkSimpleAnalysis(Env) 
  
  let value counts valNode = match valNode.value with  
    | Var id -> 
        let oldCount = Hashtbl.find_default counts id 0 in 
        Hashtbl.add counts id (oldCount+1)
    | _ -> () 
end 

module UseCountEval = MkEvaluator(UseCountAnalysis)
let find_fundef_use_counts = UseCountEval.eval_fundef