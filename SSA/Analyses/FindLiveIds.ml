(* collects the set of IDs which are used inside of a function body *) 
open Base
open SSA
open SSA_Analysis 

module LiveIdEval = MkEvaluator(struct
  type env = ID.t MutableSet.t
  type value_info = unit
  type exp_info = unit 
  
  let iterative = false
  let dir = Forward
   
  let init fundef = 
    let liveSet = MutableSet.create 127  in 
    List.iter (MutableSet.add liveSet) fundef.input_ids;   
    List.iter (MutableSet.add liveSet) fundef.output_ids;
    liveSet  
      
  let value liveSet valNode = match valNode.value with 
    | Var id -> MutableSet.add liveSet id
    | _ -> ()

  let phi_set _ _ _ = None 
  let phi_merge _ _ _ _ = None 
    
  let exp liveSet expNode helpers = helpers.iter_exp_children liveSet expNode 
  let stmt liveSet stmtNode helpers = helpers.eval_stmt liveSet stmtNode
    
end)

let find_live_ids fundef = LiveIdEval.eval_fundef fundef  