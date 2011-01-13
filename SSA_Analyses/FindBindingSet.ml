open Base 
open SSA 

module Env  = struct
  type t = ID.Set.t 
  let init fundef = ID.Set.of_list (fundef.input_ids @ fundef.output_ids)  
end
module BindingSetAnalysis = struct
  include SSA_Analysis.MkSimpleAnalysis(Env) 
  let stmt env stmtNode info = match stmtNode.stmt with 
    | Set (ids, _) -> Some (ID.Set.add_list ids env)
    | _ -> failwith "not implemented"

  
  (* let eval_stmt env src ids rhsNode rhsInfo = Some (ID.Set.add_list ids env)*)
  (* let eval_if env src *)   
end
module BindingSetEval = SSA_Analysis.MkEvaluator(BindingSetAnalysis)

let fundef_bindings fundef = BindingSetEval.eval_fundef fundef 
let block_bindings block = BindingSetEval.eval_block ID.Set.empty block    