open SSA
open Base


type direction = Forward | Backward

(* 'a = environment, *)
(* 'b = information about value nodes *)
(* 'c = information about exp nodes *)  
type ('a, 'b) helpers = { 
  eval_block : 'a -> block -> 'a * bool;
  eval_stmt : 'a -> stmt_node -> 'a option; 
  iter_stmt : 'a -> stmt_node -> unit 
  eval_values : 'a -> value_node list -> 'b list;
  iter_exp : 'a -> exp_node -> unit 
} 

module type ANALYSIS =  sig
    type env
    type exp_info
    type value_info
    
    val dir : direction
  
    val clone_env : env -> env
       
    (* should analysis be repeated until environment stops changing? *) 
    val iterative : bool
  
    val init : fundef -> env 
  
    val value : env -> value_node -> value_info
    val exp : env -> exp_node -> (env, value_info) helpers -> exp_info 
    val stmt : env -> stmt_node -> (env, value_info) helpers -> env option 
end

module MkEvaluator(A : ANALYSIS) = struct
  
  let rec eval_block initEnv block =
    let n = Block.length block in
    let changed = ref false in 
    let fold_stmts env stmtNode = 
      match eval_stmt env stmtNode with 
        | Some env' -> changed := true; env' 
        | None -> env 
    in   
    let env' = 
      match A.dir with 
      | Forward -> Block.fold_forward fold_stmts initEnv block 
      | Backward -> Block.fold_backward fold_stmts initEnv block   
    in 
    env', !changed 
    
  and eval_stmt env stmtNode = ()
  and default_stmt env stmtNode = match stmtNode.stmt with
    (* by default don't do anything to the env *) 
    | Set (ids, rhs) -> None
    | If(cond, tBlock, fBlock,  merge) ->
        let _ = eval_value env cond in  
        let tEnv, tChanged = eval_block (A.clone_env env) tBlock in 
        let fEnv, fChanged = eval_block (A.clone_env env) fBlock in
        let mergeEnv, mergeChanged = eval_merge env tEnv fEnv merge in 
        if tChanged || fChanged || mergeChanged 
        then Some mergeEnv else None     
    | WhileLoop(test, body, gates) -> failwith "no loops yet :-("
        (*let initEnv =
          ID.Map.fold 
            (fun startId (preId,_) accEnv ->
               
            
            (A.clone_env env) 
           
        let rec loop iter =
          
          if iter > 100 then failwith "loop analysis failed to terminate"
          else  
            let preEnv = A.clone_env env in
            let startEnv = 
              ID.Map.fold 
                (fun loopStartId (preId,loopEndId) acc ->
                   A.flow_merge preEnv loopStartId env     
            let merge_inputs env out
            let startEnv =  
            let condEnv, condChanged = eval_block () condBlock in
            let condInfo = eval_value condEnv condVal in 
            let
            *)   
    | _ -> failwith ("not yet implemented: " ^ (SSA.stmt_node_to_str stmtNode))
   
  and iter_stmt_children env stmtNode fExp fVal = match stmtNode with 
    | Set (_, rhs) -> fExp rhs
    | If(cond, tBlock, fBlock,  mergeBlock) ->
        fVal cond; 
        iter_block env tBlock fExp fVal; 
        iter_block env fBlock fExp fVal; 
        iter_block env mergeBlock fExp fVal
    | WhileLoop(test, body, gates) ->  
        failwith "no loops yet :-("
        (*let initEnv =
          ID.Map.fold 
            (fun startId (preId,_) accEnv ->
               
            
            (A.clone_env env) 
           
        let rec loop iter =
          
          if iter > 100 then failwith "loop analysis failed to terminate"
          else  
            let preEnv = A.clone_env env in
            let startEnv = 
              ID.Map.fold 
                (fun loopStartId (preId,loopEndId) acc ->
                   A.flow_merge preEnv loopStartId env     
            let merge_inputs env out
            let startEnv =  
            let condEnv, condChanged = eval_block () condBlock in
            let condInfo = eval_value condEnv condVal in 
            let
            *)   
  and iter_exp env expNode = match expNode.exp with 
      | App(x, xs) 
      | PrimApp(x,xs) -> A.eval_value env x; iter_values env xs   
      | Call(fnId, args) -> iter_values env args 
      | Map(closure, args) ->
          iter_values env closure.closure_args; 
          iter_values env args
      | Reduce(c1, c2, args) 
      | Scan (c1, c2, args) -> 
          iter_values env c1.closure_args;  
          iter_values env c2.closure_args;
          iter_values env args 
      | Values xs    
      | Arr xs -> iter_values env xs 
      | Phi(left,right)-> 
          A.eval_value env left; 
          A.eval_value env right 
      | _ -> failwith ("not implemented: " ^ (SSA.exp_to_str expNode))     
  and iter_values env = function 
    | [] -> ()
    | v::vs -> let _ = A.eval_value env v in iter_values f env vs 
  and eval_values env = function 
    | [] -> [] 
    | v::vs -> (A.value env v) :: (eval_values env vs) 
  and helpers = { 
      eval_block = eval_block; 
      eval_stmt = default_stmt;  
      eval_values = eval_values; 
      iter_values = iter_values; 
      iter_exp = iter_exp;  
      iter_stmt = (fun env stmtNode -> let _ = default_stmt env stmtNode in ()) 
  }
  
  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
