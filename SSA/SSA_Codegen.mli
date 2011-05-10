open SSA 

class ssa_codegen : object 
    val types : DynType.t ID.Map.t ref   
    val code : stmt_node DynArray.t 
    
    method get_type_env : DynType.t ID.Map.t 
    method get_type  : ID.t -> DynType.t  
    method add_type : ID.t -> DynType.t -> unit   
    
    method fresh_var  : DynType.t -> ID.t 
    method id_value_node : ID.t -> value_node 
    
    method cvt : to_type:DynType.t->from_type:DynType.t->value_node->value_node  
      
    method cvt_list 
      : to_type : DynType.t -> from_types:DynType.t list -> 
          value_node list ->value_node list 
        
    method emit : stmt_node list -> unit 
    
    method finalize : SSA.block  
end

val mk_codegen_fn 
      : DynType.t list ->  DynType.t list -> 
        (ssa_codegen -> value_node list-> value_node list -> unit) -> fundef 
    
    

val (:=) : value_node list -> exp_node -> stmt_node  
val (@@) : value_node -> value_node list -> exp_node   
val scalar_op : Prim.scalar_op -> value_node
val array_op : Prim.array_op -> value_node 
val impure_op : Prim.impure_op -> value_node 

val print : value_node 

val inf : value_node 
val neginf : value_node 

val plus : value_node  
val minus : value_node  
val mul : value_node 
val div : value_node 

val lt : value_node 
val lte : value_node 
val eq : value_node  

val one : value_node 
val neg_one : value_node 
val zero : value_node  

val reduce : value_node  
val map : value_node
val allPairs : value_node

val where : value_node 
val index : value_node 
val til : value_node  
val find : value_node 
val dimsize : value_node 
val select : value_node 




val len : value_node -> exp_node
val value : value_node -> exp_node
val values : value_node list -> exp_node 
 
val incr : ID.t -> value_node -> stmt_node   
val set_int : ID.t -> Int32.t -> stmt_node 

type vars = value_node array 
val  mk_fn : int -> int -> int -> 
       (vars -> vars -> vars -> stmt_node list) -> fundef

val fn1 : (value_node -> value_node -> stmt_node list) -> fundef   
