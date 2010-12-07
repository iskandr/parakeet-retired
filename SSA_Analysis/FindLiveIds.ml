(* collects the set of IDs which are used inside of a function body *) 

open Base
open SSA

class collector initSet = 
  object
    inherit SSA_Transform.default_transformation
    val liveSet : ID.t MutableSet.t = initSet 
    
    method value vNode = 
      match vNode.value with 
        | Var id -> MutableSet.add liveSet id
        | _ -> ()
      ; 
      SSA_Transform.NoChange
        
    method result = liveSet  
end  

let find_live_ids fundef =
  let liveSet = MutableSet.create 127  in 
  List.iter (MutableSet.add liveSet) fundef.input_ids;   
  List.iter (MutableSet.add liveSet) fundef.output_ids;   
  let c = new collector liveSet in 
  let _ = 
    SSA_Transform.transform_fundef (c :> SSA_Transform.transformation) fundef 
  in 
  c#result 