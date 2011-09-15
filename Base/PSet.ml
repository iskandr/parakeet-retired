  module L = BaseList 
  
  type 'a t = 'a list 
  
  let remove x set = L.filter (fun y -> x <> y) set
  let mem = L.mem 
  let empty = []
  let add x set = x::(remove x set)
  let singleton x = add x empty
  
  let enum = L.enum 
  let of_enum = L.of_enum
  
  let is_empty set = (set = []) 
  let fold = L.fold_right
  let iter = L.iter
  let map = L.map 
  let filter = L.filter  

  let for_all = L.for_all 
  let exists = L.exists 
   
  (* conversion to other data types *)
  let from_list list = list
  let to_list set = set
  let elements set = set  
  let of_list xs = xs 
  let of_array xs = (Array.to_list xs)
  let to_array xs = Array.of_list xs 
  let add_list set lst = List.fold_left (fun acc elt -> add elt acc) set lst
  (* basic set operations *)    
  let union set1 set2 = fold add set1 set2 
  let inter (set1:'a t) (set2 : 'a t) : 'a t =
    let f v acc =  if mem v set2 then add v acc else acc in 
    fold f set1 empty 
    
  let cardinal set = List.length (elements set)
  
  let equal s1 s2 = for_all (fun elt -> mem elt s2) s2
      
  let subset set1 set2 = equal (inter set1 set2) set1
    
  let diff set1 set2 = 
    let iset = inter set1 set2 in 
    let uset = union set1 set2 in 
    fold (fun x acc -> remove x acc) iset uset 

  let choose = function 
    | x::xs -> x
    | [] -> failwith "Can't choose element from empty set"
      
  let min_elt set = fold min set (choose set)
  let max_elt set = fold max set (choose set)

  type set_comparison = Disjoint | Overlap | Same 
  
  let compare_sets (set1 : 'a t) (set2 : 'a t) =
    let set3 = inter set1 set2 in  
    if is_empty set3 then Disjoint
    else if (cardinal set3) = (cardinal set1) then Same 
    else Overlap
