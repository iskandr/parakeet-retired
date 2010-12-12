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
      if deadPairs = [] then None
      else if livePairs = [] then Some [] 
      else 
        let liveIds, liveValues = List.split livePairs in
        let rhs = {expNode with exp=Values liveValues} in  
        Some {stmtNode with stmt = Set(liveIds, rhs)}      
  | Set (ids, exp) -> 
      if List.exists (MutableSet.mem liveSet) ids then None  
      else Some [] 
  | _ -> None 
 
  let exp _ _ = None  
  let value _ _ = None    
end 

let elim_dead_code =   MkTransformation(DCE_Rules).transform_fundef