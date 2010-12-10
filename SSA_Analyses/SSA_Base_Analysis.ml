open Base
open SSA

(* sketch of future SSA_Query or SSA_Analysis base class: *)
  type direction = Forward | Backward
  type gate = (ID.t * ID.t * ID.t) list 
  type 'a merge_function = 'a -> 'a -> gate -> 'a  
  type 'a flow_sensitivity = 
    | FlowInsensitive 
    | FlowSensitive of 'a merge_function 


(* generic structure of a recursive analysis, fill in the details by 
   inheriting from base_analysis
*)

class type ['a] analysis = object
  method before_fundef : 'a -> SSA.fundef -> 'a
  method stmt : 'a -> SSA.stmt_node -> 'a 
  method exp : 'a -> SSA.exp_node -> 'a 
  method value : 'a -> SSA.value_node -> 'a  
end

class ['a] default_analysis = object 
  method before_fundef env _ = env 
  method stmt env _ = env 
  method exp env _ = env 
  method value env _ = env 
end

let rec eval_block logic env code = 
  let stmt_folder (env,changed) stmt = 
    let env', changed' = eval_stmt logic env  stmt in 
    env', changed || changed'
  in  
  List.fold_left stmt_folder (env,false) code
      
and eval_stmt logic env node = 
  let env', changed = match node.stmt with
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
