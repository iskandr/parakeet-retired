(* memspace id's factored out of Mem so that they can be used*)
(* by both Mem and DataManager *) 

type t 
val gen : unit -> t 

val id_to_str : t -> string 
val set_name : t -> string -> unit
val register : string -> t 
val find_name : t -> string
val find_id : string -> t

module Map : sig include  Map.S with type key = t end  