open Base
open SSA
open SSA_Analysis 

module UseCountEval = MkEvaluator(struct 
  type env = (ID.t, int) Hashtbl.t
  type value_info = unit 
  type exp_info = unit 
  
  let dir = Forward
  let iterative = false
  
  let init fundef = 
    let env = Hashtbl.create 127 in 
    List.iter (fun id -> Hashtbl.add env id 1) fundef.output_ids; 
    env
    
  let value counts valNode = match valNode.value with  
    | Var id -> 
      let oldCount = Hashtbl.find_default counts id 0 in 
      Hashtbl.add counts id (oldCount+1)
    | _ -> ()
   
  let phi_set _ _ _ = None 
  let phi_merge _ _ _ _ = None 
     
  let exp env expNode helpers = helpers.iter_exp_children env expNode 
  let stmt env stmtNode helpers = helpers.eval_stmt env stmtNode
end)
 
let find_fundef_use_counts f = UseCountEval.eval_fn f 