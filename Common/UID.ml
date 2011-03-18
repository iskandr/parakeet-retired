open Base 

(* make a unique identifier module, with a specific to_str function and
   distinct counter from all other unique identifiers 
*)  
module Make(S : sig val to_str : int -> string end) = struct 
  type t = int 
  include S
   
  module Set = Int.Set
  module Map = Int.Map 
  module Graph = Int.Graph 

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
end