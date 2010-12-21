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
        (ssa_codegen -> value_node array -> value_node array -> unit) -> fundef 
    
    
val reduce : value_node  
val map : value_node
val inf : value_node 
val neginf : value_node 

val (:=) : ID.t list -> exp_node -> stmt_node  
val (@@) : value_node -> value_node list -> exp_node   
val scalar_op : Prim.scalar_op -> value_node
val array_op : Prim.array_op -> value_node 

type vars = value_node array 
val  mk_fn : int -> int -> int -> 
       (bodyConstructor : vars -> var -> vars -> stmt_node list) -> fundef

val fn1 : DynType.t -> DynType.t -> stmt_node list  
