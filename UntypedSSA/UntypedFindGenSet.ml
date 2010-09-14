open Base 
open SSA 

let find_gens = object 
    inherit [ID.t PSet.t] UntypedAnalysis.base_analysis 
    method stmt env stmtNode = match stmtNode.stmt with 
      | Set(ids, _) -> PSet.add_list ids env | _ -> env
end

let block_gens block = UntypedAnalysis.eval_block find_gens PSet.empty block
