open Base
open SSA

(* generic structure of a recursive analysis, fill in the details by 
   inheriting from base_analysis
*)

class ['a] base_analysis = object
    method stmt (env : 'a) (stmt : SSA.stmt_node) = env    
    method exp (env : 'a) (exp : SSA.exp_node)  = env
    method value (env : 'a) (v : SSA.value_node) = env
end


let rec eval_block logic env code = 
  let stmt_folder (env,changed) stmt = 
    let env', changed' = eval_stmt logic env  stmt in 
    env', changed || changed'
  in  
  List.fold_left stmt_folder (env,false) code
      
and eval_stmt logic env node = 
  let env', changed = match node.stmt with
  | Ignore rhs
  | Set (_, rhs) -> 
    let rhsEnv, rhsChanged = eval_exp logic env rhs in 
    logic#stmt rhsEnv node, rhsChanged  
  | If (cond, tBlock, fBlock, _) ->
    let condEnv, condChanged = eval_value logic env cond in  
    let trueEnv, tChanged = eval_block logic condEnv tBlock in 
    let falseEnv, fChanged = eval_block logic trueEnv fBlock in
    logic#stmt falseEnv node, (condChanged || tChanged || fChanged)  
  | SetIdx (_, indices, rhs)->  
    let indicesEnv, indicesChanged = eval_value_list logic env indices in 
    let rhsEnv, rhsChanged = eval_value logic indicesEnv rhs in 
    logic#stmt rhsEnv node, (indicesChanged || rhsChanged)
  
  in 
  env', changed || (env != env')      
and eval_exp logic env expNode = 
  let env', changed = match expNode.exp with 
  | Values vs -> eval_value_list logic env vs 
  | ArrayIndex(fn,args) -> eval_value_list logic env args
  | App (fn,args) -> eval_value_list logic env (fn::args) 
  | Cast(_, arg) -> eval_value logic env arg
  | Arr args -> eval_value_list logic env args   
  in
  let env'' = logic#exp env' expNode in 
  env'', changed || (env != env'')  
and eval_value logic env vNode = 
  let env', changed = match vNode.value with  
  | Lam fundef -> eval_block logic env fundef.body  
  | _ -> env, false
  in  
  let env'' = logic#value env' vNode in 
  env'', changed || (env != env'') 
and eval_value_list logic env vs =
  let val_folder (env,changed) v = 
    let env', changed' = eval_value logic env v in 
    env', changed || changed'   
  in List.fold_left val_folder (env,false) vs

(* COMMENTED OUT UNTIL WE HAVE LOOPS *) 
 (* if the value lattice is of finite height, this iteration should converge
      but put a maxiters of 100 just in case 
  *) 
(*
and iterate ?(niters = 1) env block = 
    let env', changed = eval_block env block in 
    if changed || niters > 100 then iterate ~niters:(niters + 1) env' block 
    else env, niters > 1 
*)
