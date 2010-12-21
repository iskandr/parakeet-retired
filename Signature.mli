type t 

val input_types : t -> DynType.t list 

(* create a signature where we know only the input types *)
val from_input_types : DynType.t list -> t 

val from_type : DynType.t list -> DynType.t list -> t 
val has_output_types : t -> bool 
val output_types : t -> DynType.t list 
val to_str : t -> string
