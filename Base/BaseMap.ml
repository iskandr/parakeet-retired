open BaseCommon

(* extend the builtin Map with helper functions *) 
module Make (M: ORD) = struct  
  
  include Map.Make(M)
  module MySet = BaseSet.Make(M)
 (*
    Ocaml's builtin Not_found exceptions are unhelpful for debugging, 
  *)    
  exception KeyNotFound of M.t 

  let find key map = 
    if mem key map then find key map  
    else raise (KeyNotFound key)

  let find_default elt m default = 
    if mem elt m then find elt m 
    else default
    
  let find_option elt m = 
    if mem elt m then Some (find elt m) 
    else None
    
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
    let aux k v acc = if MySet.mem k set then add k v acc else acc in 
    fold aux map empty

  (* returns keys of map as a list *) 
  let keys map = fold (fun k _ acc -> k::acc) map [] 
  
  let key_set map = MySet.of_list (keys map) 
  
  (* returns values of map as a list *) 
  let values map = fold (fun _ v acc -> v :: acc) map []
  
  let value_set map = MySet.of_list (values map) 
  
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
  
end