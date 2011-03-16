(* pp: -parser o pa_macro.cmo *)

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
    val value : env -> value_node -> value_info
    
    val exp : env -> exp_node -> (env, value_info) helpers -> exp_info 
    val stmt : env -> stmt_node -> (env, value_info) helpers -> env option
    
    val phi_set : env -> ID.t -> value_info -> env option 
    val phi_merge : env -> ID.t -> value_info -> value_info -> env option 
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
     
  and eval_loop_header ?(changed=false) envOut envIn = function 
    | [] -> envOut, changed  
    | phiNode::rest -> 
      let valInfo = A.value envIn phiNode.SSA.phi_left in
      let currEnvOut = A.phi_set envOut phiNode.SSA.phi_id valInfo in 
      let envOut' = Option.default envOut currEnvOut in
      let currChanged = changed || Option.is_some currEnvOut in   
      eval_loop_header ~changed:currChanged envOut' envIn rest
  
  and eval_phi_nodes ?(changed=false) envOut envLeft envRight = function 
    | [] -> if changed then Some envOut else None 
    | phiNode::rest -> 
      let leftInfo = A.value envLeft phiNode.SSA.phi_left in 
      let rightInfo = A.value envRight phiNode.SSA.phi_right in
      let currEnvOut = 
        A.phi_merge envOut phiNode.SSA.phi_id leftInfo rightInfo
      in 
      let envOut' = Option.default envOut currEnvOut in
      let currChanged = changed || Option.is_some currEnvOut in 
      eval_phi_nodes ~changed:currChanged envOut' envLeft envRight rest 
  
           
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
        eval_phi_nodes env tEnv fEnv merge 
    | WhileLoop(condBlock, condVal, body, header, exit) -> 
        let maxIters = 100 in
        let iter = ref 0 in
        let loopEnv = ref env in
        let changed = ref true in  
        while !changed do
          iter := !iter + 1;  
          if !iter > maxIters then 
            failwith $ "loop analysis failed to terminate"
          else (  
            let headerEnv, headerChanged =  
              if !iter = 1 then eval_loop_header !loopEnv env header
              else match eval_phi_nodes !loopEnv env !loopEnv header with 
                | None -> !loopEnv, false
                | Some newEnv -> newEnv, true 
            in  
            let condEnv, condChanged = eval_block headerEnv condBlock in
            ignore (A.value condEnv condVal);
            let bodyEnv, bodyChanged = eval_block condEnv body in 
            loopEnv := bodyEnv; 
            changed := headerChanged || condChanged || bodyChanged
          )    
        done;
        eval_phi_nodes ~changed:(!iter > 1) !loopEnv env !loopEnv exit
    | _ -> assert false 
  
  and iter_exp_children env expNode = match expNode.exp with 
      | App(x, xs) ->  ignore $ A.value env x; iter_values env xs
      | Call(_, xs) 
      | PrimApp(_,xs) 
      | Values xs    
      | Arr xs -> iter_values env xs   
      | Map(closure, args) ->
          iter_values env closure.closure_args; 
          iter_values env args
      | Reduce(c1, c2, initArgs, args) 
      | Scan (c1, c2, initArgs, args) -> 
          iter_values env c1.closure_args;  
          iter_values env c2.closure_args;
          iter_values env initArgs; 
          iter_values env args 
      | Cast(_, v) -> ignore $ A.value env v  
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
