open BaseCommon 
(*
module type S = sig 
  include Set.S 
  val add_list : elt list -> t -> t
  val remove_list : elt list -> t -> t
  val of_list : elt list -> t 
  val to_list : t -> elt list 
  val union_list : t list -> t 
  val map : (elt -> elt) -> t -> t 
end
*)
(* extend the builtin Set with helper functions *)

module Make (M: ORD)  = struct  
  include Set.Make(M) 
  (* add the elements of a list to an existing set *)
  let add_list lst set = 
    let rec aux acc = function 
    | [] -> acc
    | x::xs -> aux (add x acc) xs
    in aux set lst 
  
  let remove_list lst set =
    let rec aux acc = function 
    | [] -> acc
    | x::xs -> aux (remove x acc) xs
    in aux set lst 
   
  (* construct a set from the elements of a list *)   
  let of_list lst = add_list lst empty  
        
  let to_list set = fold BaseList.cons set [] 
    
  (* takes a list of sets, returns their union *)
  let union_list lst = BaseList.fold_left union empty lst 
  
  let map f set = 
    fold (fun elt accSet -> add (f elt) accSet) set empty 
end