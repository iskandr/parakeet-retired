open SSA
open Base


type direction = Forward | Backward

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
    
    val exp_values 
      : env -> exp_node -> 
        vs:value_node list -> info:value_info list -> exp_info
    
    val exp_arr 
      : env -> exp_node -> elts:value_node list -> 
        info:value_info list -> exp_info 
        
    val exp_primapp 
      : env -> exp_node -> prim:Prim.prim -> args:value_node list ->
        argInfo:value_info list -> exp_info 
        
    val exp_call 
      : env -> exp_node -> fnId:FnId.t -> args:value_node list -> 
        info:value_info list -> exp_info 
          
    val exp_map 
      : env -> exp_node -> closure:closure -> args:value_node list -> 
        closureInfo:value_info list -> argInfo:value_info list -> exp_info 
  
    val exp_reduce 
      : env -> exp_node -> initClosure:closure -> reduceClosure:closure -> 
          args:value_node list -> initInfo : value_info list -> 
          reduceInfo:value_info list -> argInfo:value_info list -> exp_info 
    
    val exp_scan
      : env -> exp_node -> initClosure:closure -> scanClosure:closure -> 
          args:value_node list -> initInfo : value_info list -> 
          scanInfo:value_info list -> argInfo:value_info list -> exp_info 
            
    val exp_app 
      : env -> exp_node -> fn:value_node -> args:value_node list -> 
        fnInfo : value_info -> argInfo : value_info list -> exp_info 
    
    val exp_phi 
        : env -> exp_node -> 
          left:value_node -> right:value_node -> 
          leftInfo:value_info -> rightInfo:value_info -> exp_info 
          
    val stmt_set 
        : env -> stmt_node -> ids:ID.t list -> rhs:exp_node -> 
            rhsInfo:exp_info -> env option 

    val stmt_if 
        : env -> stmt_node -> 
            cond:value_node -> tBlock:block -> fBlock:block -> merge:block -> 
            condInfo:value_info -> tEnv:env -> fEnv:env -> mergeEnv:env ->  
            env option    
end

module type INIT = sig
  type env
  val init : fundef -> env
end 

module type VALUE = sig 
  type t 
  val bottom : t  
  val exp_default : exp_node -> t list  
end

module UnitVal  = struct
  type t = unit 
  let bottom = ()
  let exp_default _ = [] 
end

module TypeVal  = struct
  type t = DynType.t
  let bottom = DynType.BottomT
  let exp_default expNode = expNode.exp_types  
end 


(* only information needed to specify a minimal imperative analysis
   which performs a no-op on every syntax node 
*)  

module MkAnalysis (Init : INIT)(V : VALUE) =  
struct
  type env = Init.env 
  type exp_info  = V.t list 
  type value_info = V.t 
  
  let dir = Forward
  let clone_env env = env  
  
  let iterative = false
  
  (* ignore the function definition and just create a fresh lattice value *)
  let init = Init.init
  
  let value (_:env) (valNode:value_node) = V.bottom  
  
  let exp_values 
        (_:env) 
        (expNode:exp_node) 
        ~(vs:value_node list) 
        ~(info:V.t list) = V.exp_default expNode
         
  let exp_app 
        (_:env) 
        (expNode:exp_node) 
        ~(fn:value_node) 
        ~(args:value_node list) 
        ~(fnInfo:V.t)
        ~(argInfo:V.t list) = V.exp_default expNode    

  let exp_arr (_:env) (expNode:exp_node) 
        ~(elts:value_node list) ~(info:value_info list) = V.exp_default expNode 
  
  let exp_primapp 
        (_:env) 
        (expNode:exp_node) 
        ~(prim:Prim.prim) 
        ~(args:value_node list) 
        ~(argInfo:value_info list) = V.exp_default expNode 
            
  let exp_call (_:env) (expNode:exp_node) 
        ~(fnId:FnId.t) ~(args:value_node list)
        ~(info:value_info list) = V.exp_default expNode  
          
  let exp_map (_:env) (expNode:exp_node) 
        ~(closure:closure) ~(args:value_node list)
        ~(closureInfo:value_info list) ~(argInfo:value_info list) = 
          V.exp_default expNode 
        
  let exp_reduce (_:env) (expNode:exp_node) 
        ~(initClosure:closure) 
        ~(reduceClosure:closure) 
        ~(args:value_node list) 
        ~(initInfo:value_info list) 
        ~(reduceInfo:value_info list) 
        ~(argInfo:value_info list) = V.exp_default expNode  
  
  let exp_scan (_:env) (expNode:exp_node) 
        ~(initClosure:closure) 
        ~(scanClosure:closure) 
        ~(args:value_node list) 
        ~(initInfo:value_info list) 
        ~(scanInfo:value_info list)
        ~(argInfo:value_info list) = V.exp_default expNode
 
               
 let exp_phi 
        (_:env) (expNode:exp_node) 
        ~(left:value_node)
        ~(right:value_node)
        ~(leftInfo:value_info)
        ~(rightInfo:value_info) = V.exp_default expNode 
                    
 let stmt_set 
        (env:env) 
        (_:stmt_node) 
        ~(ids:ID.t list) 
        ~(rhs:exp_node) 
        ~(rhsInfo:V.t list) = None
                
  
  let stmt_if 
        (_:env) 
        (_:stmt_node)
        ~(cond:SSA.value_node)
        ~(tBlock:SSA.block)
        ~(fBlock:SSA.block)
        ~(merge:SSA.block)
        ~(condInfo:V.t)
        ~(tEnv:env)
        ~(fEnv:env)
        ~(mergeEnv:env) = Some mergeEnv      
end 


module MkEvaluator(A : ANALYSIS) = struct 
  let rec eval_block initEnv block =
    let n = Block.length block in  
    let env = ref initEnv in 
    let changed = ref false in 
    match A.dir with 
    | Forward -> 
        for i = 0 to n - 1 do
          match eval_stmt !env (Block.idx block i) with
            | Some env' -> env := env'; changed := true 
            | None -> () 
        done; 
        !env, !changed 
    | Backward -> 
        for i = n - 1 downto 0 do
          match eval_stmt !env (Block.idx block i) with 
            | Some env' -> env := env'; changed := true
            | None -> ()
        done; 
        !env, !changed

  and eval_stmt env stmtNode = match stmtNode.stmt with 
    | Set (ids, rhs) -> 
        A.stmt_set env stmtNode ~ids ~rhs ~rhsInfo:(eval_exp env rhs) 
    | If(cond, tBlock, fBlock,  mergeBlock) -> 
        let condInfo = eval_value env cond in
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
     
  and eval_exp env expNode = match expNode.exp with 
      | App(fn, args) -> 
          A.exp_app env expNode 
            ~fn ~args 
            ~fnInfo:(eval_value env fn) ~argInfo:(eval_values env args)
      | PrimApp(prim, args) -> 
          A.exp_primapp env expNode ~prim ~args ~argInfo:(eval_values env args) 
      | Call(fnId, args) -> 
          A.exp_call env expNode ~fnId ~args ~info:(eval_values env args) 
      | Map(closure, args) ->
          A.exp_map env expNode 
            ~closure ~args 
            ~closureInfo:(eval_values env closure.closure_args) 
            ~argInfo:(eval_values env args)   
      
      | Reduce(initClosure, reduceClosure, args) -> 
          let initInfo = eval_values env initClosure.closure_args in 
          let reduceInfo = eval_values env reduceClosure.closure_args in 
          let argInfo = eval_values env args in 
          A.exp_reduce env expNode 
            ~initClosure ~reduceClosure ~args 
            ~initInfo ~reduceInfo ~argInfo
      
      | Scan(initClosure, scanClosure, args) -> 
          let initInfo = eval_values env initClosure.closure_args in 
          let scanInfo = eval_values env scanClosure.closure_args in 
          let argInfo = eval_values env args in 
          A.exp_scan env expNode
            ~initClosure ~scanClosure ~args 
            ~initInfo ~scanInfo ~argInfo
            
      | Values vs -> A.exp_values env expNode ~vs ~info:(eval_values env vs)    
      | Arr elts -> A.exp_arr env expNode ~elts ~info:(eval_values env elts)
      | Phi(left,right)-> 
          let leftInfo = eval_value env left in 
          let rightInfo = eval_value env right in 
          A.exp_phi env expNode ~left ~right ~leftInfo ~rightInfo
            
      | _ -> failwith ("not implemented: " ^ (SSA.exp_to_str expNode))     
  and eval_value = A.value  
  and eval_values env values = List.map (eval_value env) values 

  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
