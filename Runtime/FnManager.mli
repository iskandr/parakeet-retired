open Base 
 
val add_untyped :  ?optimize:bool -> string -> SSA.fundef -> unit  

val create_from_untyped_map : ?optimize:bool -> SSA.fundef String.Map.t -> t 
val create_from_untyped_list : ?optimize:bool -> (string * SSA.fundef) list -> t

val add_untyped_list : ?optimize:bool -> (string * SSA.fundef) list -> unit  
val add_untyped_map : ?optimize:bool ->  SSA.fundef String.Map.t -> unit
 
val add_specialization : ?optimize:bool -> SSA.value -> Signature.t -> SSA.fundef -> unit 
      
val maybe_get_specialization : SSA.value -> Signature.t -> FnId.t option 
      
val is_untyped_function : FnId.t -> bool
val get_untyped_function : FnId.t -> SSA.fundef 
val get_typed_function : FnId.t -> SSA.fundef 

val get_untyped_name : FnId.t -> string 
val get_untyped_id : string -> FnId.t 

val get_typed_function_table : unit -> FnTable.t 
val get_untyped_function_table : unit -> FnTable.t 

val have_untyped_function : string -> bool 
val get_untyped_arity : FnId.t -> int 

val optimize_typed_functions : unit -> unit
val optimize_untyped_functions : unit -> unit     