module type S = sig
  
  type t 
  val to_str : t -> string
  
  type uid = t  
  module Set : module type of Set.Make(struct type t = uid let compare = compare end)
  module Map : module type of Map.Make(struct type t = uid let compare = compare end)

  val gen : unit -> t
   
  (* takes a list of ids, returns a mapping of id -> fresh id *)
  val map_fresh : t list -> t list 
  val gen_fresh_list : int -> t list
  val gen_fresh_array : int -> t array  
end

module Make(A : sig val to_str : int -> string end) : S  