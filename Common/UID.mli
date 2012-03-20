open Base


module Make(A : sig val prefix : string end) : sig

  type t
  val get_original_prefix : t -> string
  val to_str : t -> string
  val list_to_str : ?sep:string -> t list -> string

  type uid = t
  module Set :
    module type of Set.Make(struct type t = uid let compare = compare end)

  module Map :
    module type of Map.Make(struct type t = uid let compare = compare end)

  val try_next_suffix : string -> string
  val gen_named : string -> t
  val gen_named_list : string -> int -> t list
  val gen_named_array : string -> int -> t array
  val gen : unit -> t
  val gen_named_opt : string option -> t


  val map_fresh : t list -> t Map.t
  val gen_fresh_list : int -> t list
  val gen_fresh_array : int -> t array
  val of_int : int -> t
  val to_int : t -> int
end
