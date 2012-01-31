open BaseCommon


(*
module type S = sig
  include Map.S
  module KeySet : BaseSet.S with type elt = key
  exception KeyNotFound of key
  val find : key -> 'a t -> 'a
  val find_default : key -> 'a t -> 'a -> 'a
  val find_option : key -> 'a t -> 'a option
  val find_list : key list -> 'a t -> 'a list
  val add_list : (key * 'a) list -> 'a t -> 'a t
  val remove_list : key list -> 'a t -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val of_lists : key list -> 'a list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val to_array : 'a t -> (key * 'a) array
  val to_arrays : 'a t -> ((key array) * ('a array))
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_by_key : (key -> bool) -> 'a t -> 'a t
  val filter_by_value : ('a -> bool) -> 'a t -> 'a t
  val filter_with_map :  'a t -> bool t -> 'a t
  val filter_with_set : 'a t -> KeySet.t -> 'a t
  val keys : 'a t -> key list
  val key_set : 'a t -> KeySet.t
  val values : 'a t -> 'a list
  (*val value_set
  let value_set map = KeySet.of_list (values map)*)

  val partition : (key -> 'a -> bool) -> 'a t -> ('a t) * ('a t)
  val partition_by_key : (key -> bool) -> 'a t -> ('a t) * ('a t)
  val partition_by_value : ('a -> bool) -> 'a t -> ('a t) * ('a t)
  val num_keys : 'a t -> int
  val combine : 'a t -> 'a t -> 'a t
  val extend : 'a t -> key list -> 'a list -> 'a t

end
*)

(* extend the builtin Map with helper functions *)
module Make (M: ORD) = struct

  include Map.Make(M)
  module KeySet = BaseSet.Make(M)
 (*
    Ocaml's builtin Not_found exceptions are unhelpful for debugging,
  *)
  exception KeyNotFound of key

  let find key map =
    try find key map with  _ -> raise (KeyNotFound key)

  let find_default elt m default =
    try find elt m with _ -> default

  let find_option elt m =
    try Some (find elt m) with _ -> None

  let find_list elts m =
    let rec aux = function
      | [] -> []
      | e::es ->
         let vs = aux es in
         (match find_option e m with None -> vs | Some v -> v::vs)
    in aux elts

  (* add each pair in a list to an existing map *)
  let add_list lst map =
    let rec aux acc = function
    | [] -> acc
    | (x,y)::rest -> aux (add x y acc) rest
  in aux map lst

  let remove_list lst map =
    let rec aux acc = function
    | [] -> acc
    | x::rest -> aux (remove x acc) rest
  in aux map lst

  (* construct a map from a list of pairs *)
  let of_list lst = add_list lst empty

  let of_lists l1 l2 = of_list (List.combine l1 l2)

  let to_list map = fold (fun k v acc -> (k,v)::acc)  map []

  let to_array map =
    let arr = DynArray.create () in
    iter (fun k v -> DynArray.add arr (k,v)) map;
    DynArray.to_array arr

  (** returns pair of key array and value array *)
  let to_arrays map =
    let keys = DynArray.create () in
    let values = DynArray.create () in
    iter (fun k v -> DynArray.add keys k; DynArray.add values v) map;
    DynArray.to_array keys, DynArray.to_array values

  let filter f map =
    let aux k v acc = if f k v then add k v acc else acc in
    fold aux map empty

  let filter_by_key f map =
    let aux k v acc = if f k then add k v acc else acc in
    fold aux map empty

  let filter_by_value f map =
    let aux k v acc = if f v then add k v acc else acc in
    fold aux map empty

  (* boolMap should contain all the same keys as map, providing
     a boolean membership value for each
  *)
  let filter_with_map map boolMap =
    let aux k v acc = if find k boolMap then add k v acc else acc in
    fold aux map empty

  let filter_with_set map set =
    let aux (k : key) v acc = if KeySet.mem k set then add k v acc else acc in
    fold aux map empty

  (* returns keys of map as a list *)
  let keys map = fold (fun k _ acc -> k::acc) map []


  let key_set map = KeySet.of_list (keys map)

  (* returns values of map as a list *)
  let values map = fold (fun _ v acc -> v :: acc) map []

  (*let value_set map = KeySet.of_list (values map)*)

  let partition f map =
    let aux k v (accT, accF) =
      if f k v then add k v accT, accF else accT, add k v accF in
    fold aux map (empty,empty)

  let partition_by_key f map =
    let aux k v (accT, accF) =
      if f k then add k v accT, accF else accT, add k v accF in
    fold aux map (empty,empty)

  let partition_by_value f map =
    let aux k v (accT, accF) =
      if f v then add k v accT, accF else accT, add k v accF in
    fold aux map (empty,empty)

  let num_keys map = List.length $ keys map

  let combine map1 map2 = add_list (to_list map1) map2

  let extend (map : 'a t) (keys : M.t list) (vals : 'a list) : 'a t  =
    List.fold_left2 (fun accMap key v -> add key v accMap) map keys vals
end