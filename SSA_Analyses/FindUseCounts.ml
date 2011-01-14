open Base
open SSA
open SSA_Analysis 

module Env = struct 
  type t = (ID.t, int) Hashtbl.t
  let init fundef = 
    let env = Hashtbl.create 127 in 
    List.iter (fun id -> Hashtbl.add env id 1) fundef.output_ids; 
    env  
end

module UseCountAnalysis = struct
  include MkAnalysis(Env)(ExpUnit)(ValUnit) 
  
  let value counts valNode = match valNode.value with  
    | Var id -> 
        let oldCount = Hashtbl.find_default counts id 0 in 
        Hashtbl.add counts id (oldCount+1)
    | _ -> () 
end 

module UseCountEval = MkEvaluator(UseCountAnalysis)
let find_fundef_use_counts = UseCountEval.eval_fundef