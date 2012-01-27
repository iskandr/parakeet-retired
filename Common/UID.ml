open Base

(* make a unique identifier module, with a specific to_str function and
   distinct counter from all other unique identifiers
*)
(*
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
*)
module Make(A : sig val prefix : string end)  = struct
  type t = int
  let name_to_id : (string, t) Hashtbl.t = Hashtbl.create 127
  let id_to_name : (t, string) Hashtbl.t = Hashtbl.create 127
  let original_prefixes : (t, string) Hashtbl.t = Hashtbl.create 127

  let get_original_prefix id =
    Hashtbl.find_default original_prefixes id A.prefix

  let to_str x = match Hashtbl.find_option id_to_name x with
    | Some name -> name
    | None -> "unknown_" ^ A.prefix ^ "_" ^ (string_of_int x)

  (* best guess at next suffix-- still have to check whether it's free *)
  let next_suffixes : (string, int) Hashtbl.t = Hashtbl.create 127

  let try_next_suffix (prefix:string) : string =
    let i = Hashtbl.find_default next_suffixes prefix 0 in
    Hashtbl.replace next_suffixes prefix (i+1);
    prefix ^ (string_of_int i)

  let max_id = ref 0
  let next_id () =
    let id = !max_id in
    max_id := id + 1;
    id

  let gen_named (prefix:string) : t =
    let unique_name = ref (try_next_suffix prefix) in
    while Hashtbl.mem name_to_id !unique_name do
      unique_name := try_next_suffix prefix
    done;
    let id = next_id() in
    Hashtbl.add original_prefixes id prefix;
    Hashtbl.add name_to_id !unique_name id;
    Hashtbl.add id_to_name id !unique_name;
    id

  let gen () = gen_named A.prefix

  type uid = t
  module Set = Set.Make(struct type t = uid let compare = compare end)
  module Map = Map.Make(struct type t = uid let compare = compare end)

  (* takes a list of ids, returns a mapping of id -> fresh id *)
  let map_fresh idList =
    let rec aux map = function
    | [] -> map
    | id::ids ->
        let prefix = get_original_prefix id in
        let fresh = gen_named prefix in
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
