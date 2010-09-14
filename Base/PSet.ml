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

        
  (*
  type 'a t = ('a, unit) PMap.t
    
  (* basic operators *)
  let remove x s = PMap.remove x s
  let mem (x:'a) (s:'a t) = PMap.mem x s 
  let empty = ( PMap.empty : 'a t)
  
  let add x s = PMap.add x () s
  let singleton x = add x empty 

  let enum set = Enum.map fst (PMap.enum set)
  let of_enum (enum : 'a Enum.t) : 'a t = Enum.fold add empty enum  


  
  let is_empty set =
      Std.print "in is_empty";
      let res =  Enum.is_empty (enum set) in 
      Std.print "done in is_empty";
      res 

  
  (* basic higher order functions *)
  let fold f (s:'a t) init = PMap.foldi (fun key () acc -> f key acc) s init
  let iter f s = PMap.iter (fun key () -> f key) s 
  let map f s = fold (fun k acc ->  add (f k) acc) s empty 
  let filter f set = 
    fold (fun elt acc -> if f elt then add elt acc else acc) set empty
    
  let for_all pred s = fold (fun k b -> pred k && b) s true 
  let exists pred s = fold (fun k b -> pred k || b) s false
   
  (* conversion to other data types *)
  let from_list list = List.fold_left (fun acc elt -> add elt acc) empty list
  let to_list set = fold (fun k acc -> k :: acc) set []
  let elements = to_list 
  let of_list xs = List.fold_left (fun  acc k ->add k acc) empty xs
  let of_array xs = of_list (Array.to_list xs)
  let to_array xs = Array.of_list (to_list xs) 

  (* basic set operations *)    
  let union set1 set2 = fold add set1 set2 
  let inter (set1:'a t) (set2 : 'a t) : 'a t =
    let f v acc =  if mem v set2 then add v acc else acc in 
    fold f set1 empty 
    
  let cardinal set = List.length (elements set)
  
  let equal s1 s2 = cardinal s1 = cardinal (inter s1 s2)
      
  let subset set1 set2 = equal (inter set1 set2) set1
    
  let diff set1 set2 = 
    let iset = inter set1 set2 in 
    let uset = union set1 set2 in 
    fold (fun x acc -> remove x acc) iset uset 

  let choose set = match Enum.peek (PMap.enum set) with 
      | None -> failwith "Can't choose element from empty set"
      | Some (k,v) -> k 
    
  let min_elt set = fold min set (choose set)
  let max_elt set = fold max set (choose set)

  type set_comparison = Disjoint | Overlap | Same 
  
  let compare_sets (set1 : 'a t) (set2 : 'a t) =
    Std.print "in comparison";
    let set3 = inter set1 set2 in  
    if is_empty set3 then Disjoint
    else if (cardinal set3) = (cardinal set1) then Same 
    else Overlap

  (*
    val partition : (elt -> bool) -> t -> t * t    
    val split : elt -> t -> t * bool * t
  *)
  *)