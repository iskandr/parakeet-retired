open Base
open SSA_Gates
open SSA

include ConstantLattice


let rec is_function_constant constEnv id =
  if PMap.mem id constEnv then  
    match PMap.find id constEnv with 
      | Const (Lam _) -> true
      | Const (Var id') -> is_function_constant constEnv id' 
      | _ -> false 
  else false   
 

let rec eval_block env block = 
  let stmt_folder (env,changed) stmt = 
    let env', changed' = eval_stmt env stmt in 
    env', changed || changed' 
  in 
  List.fold_left stmt_folder (env, false) block    
and eval_stmt env stmtNode = match stmtNode.stmt with 
  | Set ([id], rhs) -> 
      let rhsLatticeVal, rhsEnv, rhsChanged = eval_exp env rhs in 
      if not $ PMap.mem id env then PMap.add id rhsLatticeVal rhsEnv, true 
      else   
        let oldVal = PMap.find id env in 
        let combinedVal = join oldVal rhsLatticeVal in  
        PMap.add id combinedVal env, rhsChanged || (combinedVal <> oldVal)
   
  | If (_, tBlock, fBlock, ifGate) -> 
      let trueEnv, trueChanged = eval_block env tBlock in 
      let falseEnv, falseChanged = eval_block env fBlock in 
      let branchPairs = List.combine ifGate.true_ids ifGate.false_ids in
      let combineBranches (accEnv, accChanged) outId (trueId, falseId) = 
        let trueVal = PMap.find trueId trueEnv in 
        let falseVal = PMap.find falseId falseEnv in
        let newVal = join trueVal falseVal in 
        if PMap.mem outId env then 
          let oldVal = PMap.find outId env in
          let combinedVal = join newVal oldVal in  
          (
            if oldVal = combinedVal then (accEnv, accChanged) 
            else (PMap.add outId combinedVal accEnv, true)
          )
        else PMap.add outId newVal accEnv, true 
      in 
      let outIds = ifGate.if_output_ids in 
      let (env3, changed3) = 
        List.fold_left2 combineBranches (env, false) outIds branchPairs in 
      env3, trueChanged || falseChanged || changed3   
and eval_exp env expNode = match expNode.exp with 
  | Value v -> eval_value env v 
  | _ -> top, env, false 
  (* for now tuple projection, function application, and array indexing
     are treated as unknown operations *)
and eval_value env valNode = match valNode.value with  
  | Var id -> 
     let const = 
       if PMap.mem id env then match PMap.find id env with 
        | Const (Lam _) -> Const (Var id) 
        | k -> k 
       else failwith  
        (Printf.sprintf "unbound identifier %s in constant analysis"
           (ID.to_str id))
     in const, env, false
   | Lam fundef ->
    (* extend environment to make function input non-constant *)  
      let add_to_env accEnv id =  PMap.add id ManyValues accEnv in 
      let env' = List.fold_left add_to_env env fundef.input_ids in  
      let env'', changed  =  eval_block env' fundef.body in 
      Const (Lam fundef), env'', changed  
 
  | v -> Const v, env, false   
  

                
let rec find_constants code = 
    let env, _ = eval_block PMap.empty code in 
    env
    
(* useful for extracting the equivalence classes of IDs all referencing
   the same function. maps every function arglist/body to the list 
   of identifiers which reference this function. 
*) 
let build_function_map constEnv = 
  let add_fn_to_env id lattice accEnv = 
    match lattice with   
    | Const (Lam fundef) -> PMap.add id fundef accEnv  
    | _ ->  accEnv
  in 
  PMap.foldi add_fn_to_env constEnv PMap.empty 
  