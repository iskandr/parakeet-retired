open Base 

(* make a unique identifier module, with a specific to_str function and
   distinct counter from all other unique identifiers 
*)
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
  
module Make(A : sig val to_str : int -> string end) : S = struct 
  type t = int 
  let to_str x = A.to_str x 
  type uid = t 
  module Set = Set.Make(struct type t = uid let compare = compare end)
  module Map = Map.Make(struct type t = uid let compare = compare end)
  
  let gen : (unit -> t) = mk_gen ()
   
  (* takes a list of ids, returns a mapping of id -> fresh id *)
  let map_fresh idList =
    let rec aux map = function
    | [] -> map
    | id::ids ->
        let fresh = gen () in
        let map' = Map.add id fresh map in
        aux map' ids
    in aux Map.empty idList

  let gen_fresh_list count = 
    let rec aux acc count = 
      if count <= 0 then acc
      else 
      let acc' = (gen())::acc in 
      aux acc' (count - 1)
    in aux [] count
  
  let gen_fresh_array count = Array.of_list $ gen_fresh_list count
  
  let of_int x = x  
end
