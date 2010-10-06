open Base 
open SSA 

let find_gens = object 
    inherit [ID.Set.t] UntypedAnalysis.base_analysis 
    method stmt env stmtNode = match stmtNode.stmt with 
      | Set(ids, _) -> ID.Set.add_list ids env | _ -> env
end

let block_gens block = UntypedAnalysis.eval_block find_gens ID.Set.empty block
