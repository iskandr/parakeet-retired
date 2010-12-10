
type direction = Forward | Backward

type 'a update = NoChange | Update of 'a  
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
  val set : env -> ID.t list -> exp_node -> exp_info -> env update
  
  val if_ : env -> condVal:value_node -> condInfo:value_info -> 
              trueEnv:env -> trueChanged:bool -> 
              falseEnv:env -> falseChanged:bool -> if_gate -> env update
              
  val while_ : env -> ?condBlockEnv:env -> ?condBlockChanged:bool -> 
                 condVal:value_node -> bodyEnv:env -> bodyChanged:bool -> 
                 loop_gate -> env update  
end

(* minimal information needed for a no-op analysis 
   whose value_info and exp_info = unit 
*) 
module type ENV = sig 
  type env 
  val init : fundef -> env
end 

module MakeSimpleAnalysis(E : ENV) : ANALYSIS 

module type EVALUATOR = functor (A : ANALYSIS) -> sig 
  val eval_block : A.env -> block -> A.env 
  val eval_stmt  : A.env -> stmt_node -> A.env 
  val eval_exp : A.env -> exp_node -> A.exp_info 
  val eval_value : A.env -> value_node -> A.value_info 
  val eval_values : A.env -> value_nodes -> A.value_info list
  val eval_fundef : fundef -> A.env  
end

module MakeEvaluator(A: ANALYSIS) : EVALUATOR
