open Base
open SSA
open Printf 
open SSA_Analysis

    
module ConstantAnalysis = SSA_Analysis.MkEvaluator(struct 
  type value_info = value ConstantLattice.t
  type exp_info = value_info list  
  type env = value ConstantLattice.t ID.Map.t 
  
  let init fundef = 
    List.fold_left 
      (fun accEnv id  -> ID.Map.add id ConstantLattice.ManyValues accEnv)
      ID.Map.empty 
      fundef.input_ids
      
  let value env valNode = match valNode.value with  
    | Str _ 
    | Sym _
    | Unit
    | Num _ -> ConstantLattice.Const valNode.value  
    | Var id ->
        (try ID.Map.find id env 
         with _ -> 
          failwith $ 
            Printf.sprintf "unbound %s in constant analysis" (ID.to_str id)
        )
    | _ ->  ConstantLattice.ManyValues 
  
  let exp env expNode helpers  = match expNode.exp with 
    | Values vs -> helpers.eval_values vs 
    | _ -> List.map (fun _ -> ConstantLattice.top) expNode.exp_types expNode 

  let stmt env stmtNode helpers = match stmtNode.stmt with 
    | Set(ids, rhs) -> 
      let rhsVals = helpers.eval_exp env rhs in 
      let oldVals = 
        List.map 
          (fun id -> ID.Map.find_default id env ConstantLattice.bottom) 
          ids 
      in
      let combined = List.map2 ConstantLattice.join rhsInfo oldVals in
      if List.eq_elts oldVals combined then None 
      else 
      let env' = 
        List.fold_left2 (fun acc id v -> ID.Map.add id v acc) env ids combined
      in  
      Some env'
   | _ -> helpers.eval_stmt env stmtNode    
end)

module ConstEval = SSA_Analysis.MkEvaluator(ConstantAnalysis)
          
let find_constants fundef =
  Timing.start_timer "analysis::FindConstants"; 
  let fundef' = ConstEval.eval_fundef fundef in 
  Timing.stop_timer "analysis::FindConstants";
  fundef'  