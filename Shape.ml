(*
  every host value has a shape vector stored as a Bigarray, to give us
  easy access to the underling C data pointer.
  A scalar has a 0-element shape vector, a 1D vector has a
  1-element shape vector and a 2D vector has a 2 element shapevec.
  The i'th element of the shapevec is the length of vectors on the
  i'th level of nesting. Vectors nested on the same level all have
  to be of the same length.
*)
open Bigarray
type t = (int32, int32_elt, c_layout) Array1.t

let create : (int -> t)  = Array1.create int32 c_layout

let empty = create 0

(* the product of the elements in a shape vector is the number of elements
   in an array
*)
let nelts shape = 
  let n = Array1.dim shape in 
  let total = ref Int32.one  in 
  for i = 0 to n - 1 do 
    total := Int32.mul !total (Array1.get shape i)
  done; 
  Int32.to_int !total 

let rank shape = Array1.dim shape 

let nbytes_of_rank rank = rank * 4

let nbytes shape =  nbytes_of_rank (rank shape) 



let get_dim = Array1.get

(* shape s1 is a subshape of s2 if s1 is a scalar or 
   there is a rightmost subarray of s2 which is equal to s1 
*)
let is_subshape s1 s2 = 
  let r1, r2 = rank s1, rank s2 in
  if r1 > r2 then false 
  else if r1 = 0 then true 
  else  
    let acc = ref true in 
    for i = 0 to r1 - 1 do 
       acc := !acc && get_dim s1 i = get_dim s2 (r2 - r1 + i)
    done; 
    !acc

(* max_shape is a partial function since there can be no max between 
   unrelated shapes
*)
let max_shape s1 s2 =
  if is_subshape s1 s2 then Some s2
  else if is_subshape s2 s1 then Some s1
  else None

(* given a list of shapes, return the maximal element,
   or None if the shapes are unrelated
*) 
let max_shape_list lst =
  let aux maybeS1 s2 = match maybeS1 with 
    | None -> None
    | Some s1 -> max_shape s1 s2 
  in  
  List.fold_left aux (Some empty) lst

external get_raw_ptr : t -> HostPtr.t = "get_shapevec_ptr"

