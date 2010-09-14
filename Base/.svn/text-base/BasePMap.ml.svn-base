  open BaseCommon 
  
  include PMap 
   
  let find_default elt m default = 
  if mem elt m then find elt m 
  else default

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
  
  let to_list map = foldi (fun k v acc -> (k,v)::acc)  map []
  
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
    foldi aux map empty
  
  let filter_by_key f map = 
    let aux k v acc = if f k then add k v acc else acc in 
    foldi aux map empty
  
  let filter_by_value f map = 
    let aux k v acc = if f v then add k v acc else acc in 
    foldi aux map empty
       
  (* boolMap should contain all the same keys as map, providing
     a boolean membership value for each 
  *) 
  let filter_with_map map boolMap = 
    let aux k v acc = if find k boolMap then add k v acc else acc in 
    foldi aux map empty

  let filter_with_set map set = 
    let aux k v acc = if mem k set then add k v acc else acc in 
    foldi aux map empty

  (* returns keys of map as a list *) 
  let keys m = Enum.map fst (enum m) 
  
  (* returns values of map as a list *) 
  let values m = Enum.map snd (enum m)
  
  let partition f map = 
    let aux k v (accT, accF) = 
      if f k v then add k v accT, accF else accT, add k v accF in 
    foldi aux map (empty,empty)
  
  let partition_by_key f map = 
    let aux k v (accT, accF) = 
      if f k then add k v accT, accF else accT, add k v accF in 
    foldi aux map (empty,empty)
  
  let partition_by_value f map = 
    let aux k v (accT, accF) = 
      if f v then add k v accT, accF else accT, add k v accF in 
    foldi aux map (empty,empty)
 
  let num_keys m = Enum.count (enum m) 
 
  let combine map1 map2 = add_list (to_list map1) map2 
  
  let equal map1 map2 = 
    let aux otherMap key value accBool = 
      accBool && mem key otherMap && find key otherMap = value 
    in
    (foldi (aux map1) map2 true) && (foldi (aux map2) map1 true)
     
    