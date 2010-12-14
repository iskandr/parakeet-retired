open Base
open SSA
open Printf 
open SSA_Analysis

module ConstantAnalysis = struct
  
  type value_info = value ConstantLattice.t
  type exp_info = value_info list  
  type env = value_info ID.Map.t 
  
  let dir = Forward
  let iterative = true 
  let flow_split env = env, env 
  let flow_merge = SSA_Analysis.mk_map_merge ConstantLattice.join 
        
  let init fundef : env =
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
        if ID.Map.mem id env then ID.Map.find id env  
        else failwith  
          (Printf.sprintf "unbound identifier %s in constant analysis"
           (ID.to_str id))
    | _ ->  ConstantLattice.ManyValues 
  
  let exp env expNode = function  
    | ValuesInfo consts -> consts  
    | _ -> List.map (fun _ -> ConstantLattice.top) expNode.exp_types
  
  let stmt env stmtNode stmtInfo = match stmtNode.stmt, stmtInfo with
    (* TODO: Fix!*) 
    | Set(ids, _), SetInfo vs -> flow_merge env ids env ids env ids  
    | _ -> None    
end

module ConstEval = SSA_Analysis.MkEvaluator(ConstantAnalysis)
          
let find_constants fundef = ConstEval.eval_fundef fundef  
 