(*
  every host value has a shape vector stored as a Bigarray, to give us
  easy access to the underling C data pointer.
  A scalar has a 0-element shape vector, a 1D vector has a
  1-element shape vector and a 2D vector has a 2 element shapevec.
  The i'th element of the shapevec is the length of vectors on the
  i'th level of nesting. Vectors nested on the same level all have
  to be of the same length.
*)
open Base
open Bigarray

type t = int array 

let create n = Array.create n 0 

let get shape idx = shape.(idx)
  
let set shape idx v = shape.(idx) <- v

let rank shape = Array.length shape 


let scalar_shape = create 0

let to_str s =
  let r = rank s in   
  let b = Buffer.create (r*3) in 
  Printf.bprintf b "[";
  for i = 0 to r - 1 do 
    if i < r - 1 then 
      Printf.bprintf b "%d, " (get s i) 
    else 
      Printf.bprintf b "%d" (get s i)
  done; 
  Printf.bprintf b "]";
  Buffer.contents b


let of_list l =
  let n = List.length l in
  let s = create n in
  List.iter2 (fun dim idx -> set s idx dim) l (List.til n);
  s


(* the product of the elements in a shape vector is the number of elements
   in an array
*)
let nelts shape = Array.fold_left (fun acc x -> acc * x) 1 shape 

let nbytes shape = 4 * rank shape

let eq s1 s2 =
  (rank s1 = rank s2) && 
  (
    let are_same = ref true in
    for i = 0 to rank s1 - 1 do
      are_same := !are_same && get s1 i = get s2 i 
    done;
    !are_same
  )

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
       acc := !acc && get s1 i = get s2 (r2 - r1 + i)
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
  List.fold_left aux (Some scalar_shape) lst

 
let peel_shape shape = 
  let n = rank shape in 
  if n <= 1 then scalar_shape 
  else
    let shape' = create (n-1) in 
    for i = 0 to n - 2 do
      set shape' i (get shape (i+1)) 
    done;   
    shape'
  
let append shape1 shape2 = 
  let m = rank shape1 in 
  let n = rank shape2 in 
  let combined = create (m+n) in 
  for i = 0 to m - 1 do 
    set combined i (get shape1 i)
  done; 
  for i = m to m+n-1 do 
    set combined i (get shape2 (i - m))
  done; 
  combined 
  
let append_dim dim shape = 
  let n = rank shape in 
  let shape' = create (n+1) in
  set shape' 0 dim;  
  for i = 0 to n - 1 do 
    set shape' (i+1) (get shape i)
  done; 
  shape'  
  
let append_dims dims shape = 
  let n = rank shape in
  let extra = of_list dims in 
  append extra shape 
  
let slice_shape inputShape dimsList = 
  let n = rank inputShape in 
  let nRemoved = List.length dimsList in
  let sliceRank = n - nRemoved in
  assert (sliceRank >= 0);  
  if sliceRank = 0 then scalar_shape 
  else (
    let resultShape = create sliceRank in 
    let idx = ref 0 in 
    for i = 0 to n - 1 do
      (* if this dim hasn't been removed, add it to the result *)  
      if not (List.mem i dimsList) then begin  
        set resultShape !idx (get inputShape i);
        idx := !idx + 1
      end
    done; 
    resultShape   
 )  
let to_c_array shape =
  let n = rank shape in  
  let cArr = Array1.create int32 c_layout n in 
  for i = 0 to n - 1 do 
    cArr.{i} <- Int32.of_int (get shape i)
  done; 
  cArr
  
let of_c_array cArr = 
  let n = Array1.dim cArr in 
  let shape = create n in 
  for i = 0 to n - 1 do
    set shape  i (Int32.to_int cArr.{i})
  done;
  shape 
