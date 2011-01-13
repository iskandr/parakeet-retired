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
     
    | Set(ids, _), SetInfo currVals -> 
        let oldVals = List.map (fun id -> ID.Map.find id env) ids in
        let combined = List.map2 ConstantLattice.join currVals oldVals in
        if List.exists2 (<>) oldVals combined then 
          Some (List.fold_left2 
                 (fun accEnv id v -> ID.Map.add id v accEnv) 
                 env
                 ids 
                 combined
               )
        else None 
    | _ -> None    
end

module ConstEval = SSA_Analysis.MkEvaluator(ConstantAnalysis)
          
let find_constants fundef = ConstEval.eval_fundef fundef  
 