module type S = sig
  
  type t 
  val to_str : t -> string
  
  module Set : Set.S with type elt = t  
  module Map : Map.S with type key = t 

  val gen : unit -> t
   
  (* takes a list of ids, returns a mapping of id -> fresh id *)
  val map_fresh : t list -> t Map.t  
  
  val gen_fresh_list : int -> t list
  val gen_fresh_array : int -> t array  
  
  val of_int : int -> t 
end

module Make(A : sig val to_str : int -> string end) : S  