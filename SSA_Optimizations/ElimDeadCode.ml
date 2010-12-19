open Base
open SSA
open SSA_Transform

module DCE_Rules = struct 
  type context = ID.t MutableSet.t 
  let init fundef = FindLiveIds.find_live_ids fundef
  let dir = Forward 
  let stmt liveSet stmtNode = match stmtNode.stmt with
  | Set (ids, ({exp=Values vs} as expNode)) -> 
      let pairs = List.combine ids vs in 
      let livePairs, deadPairs = 
        List.partition (fun (id,_) -> MutableSet.mem liveSet id) pairs 
      in
      if deadPairs = [] then NoChange
      else if livePairs = [] then Some SSA.empty_stmt 
      else 
        let liveIds, liveValues = List.split livePairs in
        let rhs = {expNode with exp=Values liveValues} in  
        Update (SSA.mk_set ?src:stmtNode.stmt_src liveIds rhs)      
  | Set (ids, exp) -> 
      if List.exists (MutableSet.mem liveSet) ids then NoChange  
      else Update SSA.empty_stmt
  | _ -> NoChange
  
 
  let exp _ _ = NoChange  
  let value _ _ = NoChange   
end 
module DCE_Rewrite = MkTransformation(DCE_Rules)
let elim_dead_code _ = DCE_Rewrite.transform_fundef