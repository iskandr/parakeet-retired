open SSA
open Base


type direction = Forward | Backward

(* 'a = environment, *)
(* 'b = information about value nodes *)
(* 'c = information about exp nodes *)  
type ('a, 'b) helpers = {
  eval_block : 'a -> block -> 'a * bool;
  eval_stmt : 'a -> stmt_node -> 'a option; 
  eval_values : 'a -> value_node list -> 'b list;
  
  iter_exp_children : 'a -> exp_node -> unit;
  iter_values : 'a -> value_node list -> unit;  
} 

module type ANALYSIS =  sig
    type env
    type exp_info
    type value_info
    
    val dir : direction
  
    (* should analysis be repeated until environment stops changing? *) 
    val iterative : bool
  
    val init : fundef -> env 
    (*val phi : env -> ID.t -> env -> value_node -> env -> value_node -> value_info*)
    val value : env -> value_node -> value_info
    
    val exp : env -> exp_node -> (env, value_info) helpers -> exp_info
    val phi : env -> env -> env -> SSA.phi_node -> env option 
    val stmt : env -> stmt_node -> (env, value_info) helpers -> env option 
end

module MkEvaluator(A : ANALYSIS) = struct
  
  let rec eval_block initEnv block =
    let n = Block.length block in
    let changed = ref false in 
    let fold_stmts env stmtNode = 
      match A.stmt env stmtNode helpers with 
        | Some env' -> changed := true; env' 
        | None -> env 
    in   
    let env' = 
      match A.dir with 
      | Forward -> Block.fold_forward fold_stmts initEnv block 
      | Backward -> Block.fold_backward fold_stmts initEnv block   
    in 
    env', !changed 
  and default_stmt env stmtNode = match stmtNode.stmt with
    (* by default don't do anything to the env *) 
    | Set (ids, rhs) ->
        (* evaluate rhs for possible side effects *)  
        let _  = A.exp env rhs helpers in 
        None 
    | If(cond, tBlock, fBlock,  merge) ->
        let cond' = A.value env cond in  
        let tEnv, tChanged = eval_block env tBlock in 
        let fEnv, fChanged = eval_block env fBlock in
        let mergeEnv, mergeChanged = 
          List.fold_left 
            (fun (accEnv,changed) phiNode -> 
              match A.phi accEnv tEnv fEnv phiNode with 
              | None -> (accEnv, changed)
              | Some env' -> env', true 
            ) 
            (env,false) 
            merge    
        in
        if mergeChanged || tChanged || fChanged then Some mergeEnv else None 
        
    (* TODO: fix loops *) 
    | WhileLoop(condBlock, condVal, body, header, exit) -> None 
      (* 
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
   
  and iter_exp_children env expNode = match expNode.exp with 
      | App(x, xs) ->  A.value env x; iter_values env xs
      | Call(_, xs) 
      | PrimApp(_,xs) 
      | Values xs    
      | Arr xs -> iter_values env xs   
      | Map(closure, args) ->
          iter_values env closure.closure_args; 
          iter_values env args
      | Reduce(c1, c2, args) 
      | Scan (c1, c2, args) -> 
          iter_values env c1.closure_args;  
          iter_values env c2.closure_args;
          iter_values env args 
     
      | _ -> failwith ("not implemented: " ^ (SSA.exp_to_str expNode))     
  and iter_values env = function 
    | [] -> () | v::vs -> let _ = A.value env v in iter_values env vs 
  and eval_values env = function 
    | [] -> [] | v::vs -> (A.value env v) :: (eval_values env vs)
  and helpers = { 
   
      eval_block = eval_block; 
      eval_stmt = default_stmt;  
      eval_values = eval_values; 
      iter_values = iter_values; 
      iter_exp_children = iter_exp_children;  
  } 
  
  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
