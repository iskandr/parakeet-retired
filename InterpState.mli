open Base 

type t 

val add_specialization : t -> SSA.value -> Signature.t -> SSA.fundef -> unit 
      
val maybe_get_specialization : 
  t -> SSA.value -> Signature.t -> FnId.t option 
      
val get_untyped_function : t -> FnId.t -> SSA.fundef 

val get_typed_function : t -> FnId.t -> SSA.fundef 

val optimize_typed_functions : t -> unit

val optimize_untyped_functions : t -> unit     

val create_untyped : 
      FnId.t String.Map.t -> SSA.fundef FnId.Map.t -> t 
      
val get_untyped_name : t -> FnId.t -> string 
val get_untyped_id : t -> string -> FnId.t 

val get_typed_function_table : t -> FnTable.t 
val get_untyped_function_table : t -> FnTable.t 