open Base
open SSA
open SSA_Analysis 

module UseCountAnalysis = SSA_Analysis.MkAnalysis(struct 
  type env = (ID.t, int) Hashtbl.t
  
  type value_info = unit 
  type exp_info = unit 
  
  let dir = Forward
  let iterative = false
  let clone_env env = env 
  
  let init fundef = 
    let env = Hashtbl.create 127 in 
    List.iter (fun id -> Hashtbl.add env id 1) fundef.output_ids; 
    env
    
  
  let value counts valNode = match valNode.value with  
    | Var id -> 
      let oldCount = Hashtbl.find_default counts id 0 in 
      Hashtbl.add counts id (oldCount+1)
    | _ -> ()

  let exp env expNode helpers = helpers.visit_children expNode 
  let stmt env stmtNode helpers = helpers.visit_children stmtNode
end)
 
module UseCountEval = MkEvaluator(UseCountAnalysis)
let find_fundef_use_counts = UseCountEval.eval_fundef