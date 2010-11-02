open Base 
open SSA 

let find_gens = object 
    inherit [ID.Set.t] SSA_Base_Analysis.base_analysis 
    method stmt env stmtNode = match stmtNode.stmt with 
      | Set(ids, _) -> ID.Set.add_list ids env | _ -> env
end

let block_gens block = 
  SSA_Base_Analysis.eval_block find_gens ID.Set.empty block
