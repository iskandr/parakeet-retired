open Base
open SSA

let use_counts = object 
    inherit [int ID.Map.t] SSA_Base_Analysis.base_analysis
    method value counts vNode = match vNode.value with  
    | Var id -> 
      if ID.Map.mem id counts then 
        ID.Map.add id (ID.Map.find id counts + 1) counts
      else ID.Map.add id 1 counts 
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