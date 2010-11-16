open Base 

type t 

val create : unit -> t 
val add_untyped : t ->  ?opt_queue:bool -> string -> SSA.fundef -> unit  

val create_from_untyped_map : ?opt_queue:bool -> SSA.fundef String.Map.t -> t 
val create_from_untyped_list : ?opt_queue:bool -> (string * SSA.fundef) list -> t

val add_untyped_list : t -> ?opt_queue:bool -> (string * SSA.fundef) list -> unit  
val add_untyped_map : t -> ?opt_queue:bool ->  SSA.fundef String.Map.t -> unit
 
val add_specialization : t -> SSA.value -> Signature.t -> SSA.fundef -> unit 
      
val maybe_get_specialization : 
  t -> SSA.value -> Signature.t -> FnId.t option 
      
val get_untyped_function : t -> FnId.t -> SSA.fundef 
val get_typed_function : t -> FnId.t -> SSA.fundef 

val get_untyped_name : t -> FnId.t -> string 
val get_untyped_id : t -> string -> FnId.t 

val get_typed_function_table : t -> FnTable.t 
val get_untyped_function_table : t -> FnTable.t 

val have_untyped_function : t -> string -> bool 
val get_untyped_arity : t -> FnId.t -> int 

val optimize_typed_functions : t -> unit
val optimize_untyped_functions : t -> unit     

      
