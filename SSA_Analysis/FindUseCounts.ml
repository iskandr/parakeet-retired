open Base
open SSA

let use_counts = object 
    inherit [int ID.Map.t] SSA_Base_Analysis.base_analysis
    method value counts vNode = match vNode.value with  
    | Var id -> 
      let oldCount = ID.Map.find_default id counts 0 in 
      ID.Map.add id (oldCount+1) counts  
    | _ -> counts 
end


let find_block_use_counts ?(init_counts=ID.Map.empty) block = 
  SSA_Base_Analysis.eval_block use_counts init_counts block
 
let find_fundef_use_counts fundef =
  let initCounts = 
    List.fold_left 
      (fun counts outputId -> ID.Map.add outputId 1 counts) 
      ID.Map.empty
      fundef.output_ids
  in 
  find_block_use_counts ~init_counts:initCounts fundef.body  