open Base
open SSA
open SSA_Transform

module DCE_Rules = struct 
  type env = ID.t MutableSet.t 
  let init fundef = FindLiveIds.find_live_ids fundef
  
  let stmt liveSet stmtNode = match stmtNode.stmt with
  | Set (ids, ({exp=Values vs} as expNode)) -> 
      let pairs = List.combine ids vs in 
      let livePairs, deadPairs = 
        List.partition (fun (id,_) -> MutableSet.mem liveSet id) pairs 
      in
      if deadPairs = [] then NoChange 
      else 
        let liveIds, liveValues = List.split livePairs in
        let rhs = {expNode with exp=Values liveValues} in  
        Update {stmtNode with stmt = Set(liveIds, rhs)}      
  | Set (ids, exp) -> 
      if List.exists (MutableSet.mem liveSet) ids then NoChange 
      else Update SSA.empty_stmy 
  | _ -> NoChange 
 
  let exp _ _ = NoChange 
  let value _ _ = NoChange   
end 

module DCE_Transform = SSA_Transform.MkTransformation(DCE_Rules)

let elim_dead_code fnTable fundef = DCE_Transform.transform_fundef fundef  