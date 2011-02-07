open Base
open SSA
open Printf 
open SSA_Analysis

module ConstEnv = struct 
  type t = value ConstantLattice.t ID.Map.t 
  type value = SSA.value ConstantLattice.t 
  
  let init fundef = 
    List.fold_left 
      (fun accEnv id  -> ID.Map.add id ConstantLattice.ManyValues accEnv)
      ID.Map.empty 
      fundef.input_ids
      
  let add env id value = ID.Map.add id value env 
  let mem env id = ID.Map.mem id env
  let find env id = ID.Map.find id env  
end 

module ValInfo = struct 
  type t = value ConstantLattice.t 
  let combine (x:t) (y:t) = ConstantLattice.join x y  
  let bottom = ConstantLattice.bottom
  let eq = (=)  
  let exp_default expNode = 
    List.map (fun _ -> ConstantLattice.top) expNode.exp_types 
end
      
module ConstantAnalysis = struct
  include SSA_Analysis.MkAnalysis(ConstEnv)(ValInfo) 
  
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
  
  let exp_values env expNode ~vs ~info = info 
  
  let stmt_set env stmtNode ~ids ~rhs ~rhsInfo =
    let oldVals = 
      List.map (fun id -> ID.Map.find_default id env ConstantLattice.bottom) ids 
    in
    let combined = List.map2 ConstantLattice.join rhsInfo oldVals in
    if List.exists2 (<>) oldVals combined then
      let env' = 
        List.fold_left2 (fun acc id v -> ID.Map.add id v acc) env ids combined
      in  
      Some env' 
    else None
    
  end

module ConstEval = SSA_Analysis.MkEvaluator(ConstantAnalysis)
          
let find_constants fundef = ConstEval.eval_fundef fundef  