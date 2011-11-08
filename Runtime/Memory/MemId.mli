(* memspace id's factored out of Mem so that they can be used*)
(* by both Mem and DataManager *) 

type t 
val gen : unit -> t 

val set_name : t -> string -> unit
val find_name : t -> string

module Map : sig include  Map.S with type key = t end  