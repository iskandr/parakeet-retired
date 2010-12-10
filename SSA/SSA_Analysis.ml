open SSA

type direction = Forward | Backward

type gate = (ID.t * ID.t * ID.t) list

type 'a flow_functions = { 
  merge : 'a -> 'a -> gate -> 'a; 
  split: 'a -> 'a * 'a; 
} 

module type ANALYSIS = sig 
  type env 
  type exp_info 
  type value_info 
  
  val dir : direction
  
  (* if env_helpers is None then perform flow insensitive analysis *)
  val flow_functions : (env flow_functions) option  
    
  (* should analysis be repeated until environment stops changing? *) 
  val iterative : bool
  
  val init : fundef -> env 
  
  (* VALUES *) 
  val var : env -> ID.t -> vInfo 
  val num : env -> PQNum.num -> vInfo 
  
  (* EXPRESSIONS *) 
  val app : env -> value_node -> value_info -> 
              value_nodes -> value_info list -> exp_info
          
  val array : env -> value_nodes -> value_info list -> exp_info
  val values : env -> value_node list -> value_info list -> exp_info 
  
  (* STATEMENTS *) 
  val set : env -> ID.t list -> exp_node -> exp_info -> env option
  
  val if_ : env -> condVal:value_node -> condInfo:value_info -> 
              trueEnv:env -> trueChanged:bool -> 
              falseEnv:env -> falseChanged:bool -> if_gate -> env option
              
  val while_ : env -> ?condBlockEnv:env -> ?condBlockChanged:bool -> 
                 condVal:value_node -> bodyEnv:env -> bodyChanged:bool -> 
                 loop_gate -> env option  
end

(* only information needed to specify a minimal imperative analysis
   which performs a no-op on every syntax node 
*)  
module type ENV = sig 
  type env 
  val init : fundef -> env
end 

module MakeSimpleAnalysis(E : ENV) : ANALYSIS = struct
  type env = A.env  
  type exp_info = unit 
  type value_info = unit
  
  let dir = Forward
  let flow_functions = None  
   
  let iterative = false
  
  let  init = A.init 
  
  (* VALUES *) 
  let var env id = () 
  let num env n = () 
  
  (* EXPRESSIONS *) 
  let app env fNode fInfo argNode argInfos = ()
  let array env  vNodes vInfos = ()
  let values env vNode vInfos = () 
  
  (* STATEMENTS *) 
  let set env ids rhsNode rhsInfo = None
  let if_ env 
          ~condVal ~condInfo 
          ~trueEnv ~trueChanged ~falseEnv ~falseChanged ifGate = None 
              
  let while_ env ~condBlockEnv ~condBlockChanged ~condVal
                 ~bodyEnv ~bodyChanged loopGate = None 
end 


module type EVALUATOR = functor (A : ANALYSIS) -> sig 
  val eval_block : A.env -> block -> A.env 
  val eval_stmt  : A.env -> stmt_node -> A.env 
  val eval_exp : A.env -> exp_node -> A.exp_info 
  val eval_value : A.env -> value_node -> A.value_info 
  val eval_values : A.env -> value_nodes -> A.value_info list 
end

module MakeEvaluator(A : ANALYSIS) : EVALUATOR = struct
  let rec eval_block initEnv block =
    let n = block_length block in  
    let env = ref initEnv in 
    let changed = ref false in 
    match A.dir with 
    | Forward -> 
        for i = 0 to n - 1 do
          match eval_stmt !env (block_idx block i) with
            | Some env' -> env := env'; changed := true 
            | None -> () 
        done; 
        !env, !changed 
    | Backward -> 
        for i = n - 1 downto 0 do
          match eval_stmt !env (block_idx block i) with 
            | Some env' -> env := env'; changed := true
            | None -> ()
        done; 
        !env, !changed

  and eval_stmt env stmtNode = match stmtNode.stmt with 
  | Set (ids, rhs) -> 
      let rhsInfo = eval_exp env rhs in 
      A.set  env ids rhs rhsInfo
  | _ -> failwith "not yet implemented"
  and eval_exp env expNode = match expNode.exp with 
  | App(fn, args) -> 
      let fnInfo = eval_value env fn in
      let argInfos = eval_values env args in 
      A.app env fn fnInfo args argInfos 
  | _ -> failwith "not implemented"  
  and eval_value env valNode = match valNode.value with 
  | Var id -> A.var env id 
  | Num n -> A.num env n 
  | _ -> failwith "not implemented"
  and eval_values env values = List.map (eval_value env) values 

  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
