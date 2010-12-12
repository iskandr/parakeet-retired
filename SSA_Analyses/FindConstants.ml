open Base
open SSA
open Printf 

module ConstantAnalysis = struct
  type value_info = value ConstantLattice.t
  type exp_info = value_info list  
  type env = value_info ID.Map.t 
  
  let init fundef : env =
    List.fold_left 
      (fun accEnv id  -> ID.Map.add id ConstantLattice.ManyValues accEnv)
        ID.Map.empty 
        fundef.input_ids 
  
  let value env value = match value with  
    | Str _ 
    | Sym _
    | Unit
    | Num _ -> ConstantLattice.Const value 
    | Var id ->
        if ID.Map.mem id env then ID.Map.find id env  
        else failwith  
          (Printf.sprintf "unbound identifier %s in constant analysis"
           (ID.to_str id))
    | _ ->  ConstantLattice.ManyValues 

  let mk_top_list = List.map (fun _ -> ConstantLattice.top) 
  (* EXPRESSIONS *) 
  let values env expNode valConsts = valConsts 
  let array env expNode valConsts = mk_top_list expNode.exp_types   
  let app env expNode _ _ = mk_top_list expNode.exp_types  
  let cast env expNode _ = mk_top_list expNode.exp_types  
  let call _ _ _ _ = mk_top_list expNode.exp_types  
  let primapp _ _ _ _ = mk_top_list expNode.exp_types  
  let map _ _ = mk_top_list expNode.exp_types  
  let reduce _ _ = mk_top_list expNode.exp_types  
  let scan _ _ = mk_top_list expNode.exp_types  
    
              
  (* STATEMENTS *)
   
    val set : env -> ID.t list -> exp_node -> exp_info -> env option 
    val if_ : env -> (value_info, env) if_descr -> env option               
    val loop : env -> (value_info, env) loop_descr -> env option  

  let values 
    | Values vs -> eval_value_list env vs
  | _ -> 
    let vals = List.map (fun _ -> ConstantLattice.top) expNode.exp_types in 
    vals, env, false 
  
      let rhsLatticeVals, rhsEnv, rhsChanged = eval_exp env rhs in
      if List.length ids <> List.length rhsLatticeVals then 
        failwith $ sprintf "%s (%d) %s (%d)"
          "Mismatch between number of identifiers on lhs"
          (List.length ids)
          "and types on rhs"
          (List.length rhsLatticeVals)
          
      else 
      let folder (env, changed) id rhsLatticeVal =   
          let oldVal = ID.Map.find_default id env ConstantLattice.bottom in 
          let combinedVal = ConstantLattice.join oldVal rhsLatticeVal in
          let combinedChanged =  changed || (combinedVal <> oldVal) in 
          ID.Map.add id combinedVal env, combinedChanged
      in   
      List.fold_left2 folder (rhsEnv, rhsChanged) ids rhsLatticeVals 
  | SetIdx (id,_,_) -> 
      if not $ ID.Map.mem id env then 
        failwith "setting range of undefined array"
      else if ID.Map.find id env = ConstantLattice.top then env, false
      else ID.Map.add id ConstantLattice.top env, true
   
  | If (_, tBlock, fBlock, ifGate) -> 
      let trueEnv, trueChanged = eval_block env tBlock in 
      let falseEnv, falseChanged = eval_block env fBlock in 
      let branchPairs = List.combine ifGate.true_ids ifGate.false_ids in
      let combineBranches (accEnv, accChanged) outId (trueId, falseId) = 
        let trueVal = ID.Map.find trueId trueEnv in 
        let falseVal = ID.Map.find falseId falseEnv in
        let newVal = ConstantLattice.join trueVal falseVal in 
        if ID.Map.mem outId env then 
          let oldVal = ID.Map.find outId env in
          let combinedVal = ConstantLattice.join newVal oldVal in  
          (
            if oldVal = combinedVal then (accEnv, accChanged) 
            else (ID.Map.add outId combinedVal accEnv, true)
          )
        else ID.Map.add outId newVal accEnv, true 
      in 
      let outIds = ifGate.if_output_ids in 
      let (env3, changed3) = 
        List.fold_left2 combineBranches (env, false) outIds branchPairs in 
      env3, trueChanged || falseChanged || changed3   
  (* for now tuple projection, function application, and array indexing
     are treated as unknown operations *)
                
let find_constants fundef =
 