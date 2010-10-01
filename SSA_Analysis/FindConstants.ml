open Base
open SSA
open SSA_Gates
open Printf 

module M = ID.Map 

let rec is_function_constant constEnv id =
  if ID.Map.mem id constEnv then  
    match ID.Map.find id constEnv with 
      | ConstantLattice.Const (Lam _) -> true
      | ConstantLattice.Const (Var id') -> is_function_constant constEnv id' 
      | _ -> false 
  else false   
 

let rec eval_block env block = 
  let stmt_folder (env,changed) stmt = 
    let env', changed' = eval_stmt env stmt in 
    env', changed || changed' 
  in 
  List.fold_left stmt_folder (env, false) block    
and eval_stmt env stmtNode = 
  match stmtNode.stmt with
    | Set (ids, rhs) -> 
      let rhsLatticeVals, rhsEnv, rhsChanged = eval_exp env rhs in
      if List.length ids <> List.length rhsLatticeVals then 
        failwith $ sprintf "%s (%d) %s (%d)"
          "Mismatch between number of identifiers on lhs"
          (List.length ids)
          "and types on rhs"
          (List.length rhsLatticeVals)
          
      else 
      let folder (env, changed) id rhsLatticeVal = 
        (*if not $ ID.Map.mem id env then ID.Map.add id rhsLatticeVal env, true 
        else
        *)   
          let oldVal = ID.Map.find_default id env ConstantLattice.bottom in 
          let combinedVal = ConstantLattice.join oldVal rhsLatticeVal in  
          ID.Map.add id combinedVal env, changed || (combinedVal <> oldVal)
      in   
      List.fold_left2 folder (rhsEnv, rhsChanged) ids rhsLatticeVals 
      
  | Ignore rhs -> 
      let _, rhsEnv, rhsChanged = eval_exp env rhs in
      rhsEnv, rhsChanged 
  | SetIdx (id,_,_) -> 
      if not $ ID.Map.mem id env then failwith "setting range of undefined array"
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
and eval_exp env expNode = match expNode.exp with  
  | Values vs -> eval_value_list env vs
  | _ -> 
    let vals = List.map (fun _ -> ConstantLattice.top) expNode.exp_types in 
    vals, env, false 
  (* for now tuple projection, function application, and array indexing
     are treated as unknown operations *)
and eval_value env valNode = match valNode.value with  
  | Var id -> 
     let const = 
       if ID.Map.mem id env then match ID.Map.find id env with 
        | ConstantLattice.Const (Lam _) -> ConstantLattice.Const (Var id) 
        | k -> k 
       else failwith  
        (Printf.sprintf "unbound identifier %s in constant analysis"
           (ID.to_str id))
     in const, env, false
   | Lam fundef ->
    (* extend environment to make function input non-constant *)  
      let add_to_env accEnv id =  
        ID.Map.add id ConstantLattice.ManyValues accEnv 
      in 
      let env' = List.fold_left add_to_env env fundef.input_ids in  
      let env'', changed  =  eval_block env' fundef.body in 
      ConstantLattice.Const (Lam fundef), env'', changed  
 
  | v -> ConstantLattice.Const v, env, false   

and eval_value_list env = function 
  | [] -> [], env, false 
  | v::vs -> 
      let v', env', currChanged = eval_value env v in 
      let vs', env'', restChanged = eval_value_list env' vs in 
      v'::vs', env'', currChanged || restChanged 

                
let rec find_constants ?(free_vars = []) code =
    let initEnv = 
      List.fold_left 
        (fun accEnv id  -> ID.Map.add id ConstantLattice.ManyValues accEnv)
        ID.Map.empty 
        free_vars 
    in  
    let env, _ = eval_block initEnv code in
    let pair_to_str (id,c) = 
      (ID.to_str id) ^ " : " ^ (ConstantLattice.const_to_str c)
    in
    let env_to_str env =
      "{ " ^ (String.concat "; " (List.map pair_to_str (ID.Map.to_list env))) ^"}"
    in   
    debug $ env_to_str env; 
    env
    
(* useful for extracting the equivalence classes of IDs all referencing
   the same function. maps every function arglist/body to the list 
   of identifiers which reference this function. 
*) 
let build_function_map constEnv = 
  let add_fn_to_env id lattice accEnv = 
    match lattice with   
    | ConstantLattice.Const (Lam fundef) -> ID.Map.add id fundef accEnv  
    | _ ->  accEnv
  in 
  ID.Map.fold add_fn_to_env constEnv ID.Map.empty 
  