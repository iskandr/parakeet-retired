type t 

val inputs : t -> Type.t Args.actual_args 

(* create a signature where we know only the input types *)
val from_input_types : Type.t list -> t 
val from_args : ('a, Type.t) Args.formal_args ->  t 

val with_outputs : Type.t Args.actual_args -> Type.t list -> t 
 
val has_output_types : t -> bool 
val output_types : t -> Type.t list 
val output_types_option : t -> Type.t list option 
val to_str : t -> string
