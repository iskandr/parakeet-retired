type t 

val input_types : t -> DynType.t list 

(* create a signature where we know only the input types *)
val from_input_types : DynType.t list -> t 

val from_types : DynType.t list -> DynType.t list -> t 
val has_output_types : t -> bool 
val output_types : t -> DynType.t list 
val output_types_option : t -> DynType.t list option 
val to_str : t -> string
val append_input_types : t -> DynType.t list -> t 
val prepend_input_types : DynType.t list ->  t -> t 