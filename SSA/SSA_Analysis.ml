open SSA
open Base


type direction = Forward | Backward

(* 'a = environment, 'b = information about value nodes *) 
type ('a, 'b) exp_helpers = { 
  iter_children : ('a -> value_node -> bool) -> bool; 
  eval_block : 'a -> block -> 'a;
  eval_values : ('a -> value_node list -> 'b list) 
} 

type 'a stmt_helpers = { 
  eval_block : ('a -> block -> 'a * bool);
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
    val exp : env -> exp_node -> exp_info 
    val stmt : env -> stmt_node -> env option 
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
    
  and eval_stmt env stmtNode = match stmtNode.stmt with
    (* by default don't do anything to the env *) 
    | Set (ids, rhs) -> env, false
    | If(_, tBlock, fBlock,  mergeBlock) -> 
        let tEnv, tChanged = eval_block (A.clone_env env) tBlock in 
        let fEnv, fChanged = eval_block (A.clone_env env) fBlock in
        let mergeEnv, mergeChanged = eval_block env mergeBlock in
        begin match 
          A.stmt_if env stmtNode 
            ~cond ~tBlock ~fBlock ~merge:mergeBlock 
            ~condInfo ~tEnv ~fEnv ~mergeEnv  
        with 
          | None ->
            (* since None indicates no change was made we can only*)
            (* propagate it if none of the operations on child nodes*)
            (* also didn't involve changes *)
            if tChanged || fChanged || mergeChanged 
            then Some mergeEnv else None     
         | modified -> modified
        end  
        
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
    | _ -> failwith ("not yet implemented: " ^ (SSA.stmt_node_to_str stmtNode))
     
  and mk_stmt_helpers env stmtNode = { 
    iter_stmt_children = (iter_stmt_children env stmtNode); 
  }
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
  and eval_exp env expNode = 
    let expHelpers = mk_exp_helpers env expNode in 
    A.exp env expNode expHelpers 
  and mk_exp_helpers env expNode = {
     iter_children = iter_exp env expNode;
     eval_block = eval_block; 
     eval_values = eval_values; 
  } 
  and iter_exp env expNode f = match expNode.exp with 
      | App(x, xs) 
      | PrimApp(x,xs) -> f env x; iter_values f env xs   
      | Call(fnId, args) -> iter_values f env args 
      | Map(closure, args) ->
          iter_values f env closure.closure_args; 
          iter_values f env args
      | Reduce(c1, c2, args) 
      | Scan (c1, c2, args) -> 
          iter_values f env c1.closure_args;  
          iter_values f env c2.closure_args;
          iter_values f env args 
      | Values xs    
      | Arr xs -> iter_values f env xs 
      | Phi(left,right)-> f env left; f env right 
      | _ -> failwith ("not implemented: " ^ (SSA.exp_to_str expNode))     
  and iter_values f env = function 
    | [] -> ()
    | v::vs -> (f env v); iter_values f env vs 
  and eval_value env valNode = A.value env valNode    
  and eval_values env = function 
    | [] -> [] 
    | v::vs -> (eval_value env v) :: (eval_values env vs) 
    
  
  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
