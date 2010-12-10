
type direction = Forward | Backward

type 'a update = NoChange | Update of 'a  
type gate = (ID.t * ID.t * ID.t) list

type 'a flow_functions = { 
  merge : 'a -> 'a -> gate -> 'a; 
  split: 'a -> 'a * 'a; 
}
 
type 'a scan_info = { 
  scan_init_closure : closure; 
  scan_init_info : 'a list; 
  scan_combine_closure : closure; 
  scan_combine_info : 'a list; 
  scan_args : value_nodes; 
  scan_arg_info : 'a list; 
}

type 'a reduce_info = { 
  reduce_closure : closure; 
  reduce_closure_info : 'a list; 
  reduce_args : value_nodes; 
  reduce_arg_info : 'a list;  
}

type 'a map_info = { 
  map_closure : closure; 
  map_closure_info : 'a list; 
  map_args : value_nodes; 
  map_arg_info : 'a list; 
} 


module type ANALYSIS =  sig
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
    val var : env -> ID.t -> value_info 
    val num : env -> PQNum.num -> value_info 
    val globalfn : env -> FnId.t -> value_info
    val prim : env -> Prim.prim -> value_info  
    val str : env -> string -> value_info 
    val sym : env -> string -> value_info 
    val unit : env -> value_info 
    val lam : env -> fundef -> value_info   
  
    (* EXPRESSIONS *) 
    val values : env -> value_nodes -> value_info list -> exp_info  

    val array : env -> value_nodes -> value_info list -> exp_info 
    val app : env -> value_node -> value_info -> 
              value_nodes -> value_info list -> exp_info 
              
    val array_index : env -> value_node -> value_info -> 
                      value_nodes -> value_info list -> exp_info  
                       
    val cast : env -> DynType.t -> value_node -> value_info -> exp_info 
   
    val call : env -> typed_fn -> value_nodes -> value_info list -> exp_info 
              
    val primapp 
        : env -> typed_prim -> value_nodes -> value_info list -> exp_info
        
    val map : env -> value_info map_descr -> exp_info      
    val reduce : env -> value_info reduce_descr -> exp_info  
    val scan : env -> value_info scan_descr -> exp_info   
    
              
  (* STATEMENTS *) 
    val set : env -> ID.t list -> exp_node -> exp_info -> env option 
    val if_ : env -> (value_info, env) if_descr -> env option               
    val loop : env -> (value_info, env) loop_descr -> env option  
end


module type HAS_DEFAULT = sig 
  type t 
  val default : t 
end 
module MutableIdSet : HAS_DEFAULT 

module type LATTICE = sig 
  type t 
  val bottom : t  
  val combine : t -> t -> t
  val eq : t -> t -> bool 
end


module UnitLattice : LATTICE 
module TypeLattice : LATTICE 
module MkListLattice(L: LATTICE) : LATTICE 
module TypeListLattice : LATTICE  

module MkDefaultAnalysis : 
  functor (S : HAS_DEFAULT) -> 
  functor (E:LATTICE) ->
  functor (V:LATTICE) -> ANALYSIS  
     
module type EVALUATOR = functor (A : ANALYSIS) -> sig 
  val eval_block : A.env -> block -> A.env 
  val eval_stmt  : A.env -> stmt_node -> A.env 
  val eval_exp : A.env -> exp_node -> A.exp_info 
  val eval_value : A.env -> value_node -> A.value_info 
  val eval_values : A.env -> value_nodes -> A.value_info list
  val eval_fundef : fundef -> A.env  
end

module MkEvaluator(A: ANALYSIS) : EVALUATOR
