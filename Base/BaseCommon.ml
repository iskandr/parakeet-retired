
include Std


open SourceInfo


let ($) f x = f x
let (>>=) x f = f x

let ignore x = ()

let compose f g = fun x -> f (g x)

let debug msg = 
  IFDEF DEBUG THEN Printf.printf "%s\n" msg; END;
  ()

exception SourcedError of string * source_info
exception StaticError of string

(*
    returns a function unit -> int, which increments its internal counter
    after every call
*)
let mk_gen () =
    let curr = ref 0 in
    fun () -> let x = !curr in (curr := x + 1; x)

let all_pairs f xs ys =
    let rec aux xs acc = match xs with 
        | [] -> acc
        | x::xs' ->
            let acc' = (List.map (f x) ys)::acc in
        aux xs' acc'
    in List.concat (aux xs [])

module type ORD = sig
  type t
  val compare : t -> t -> int
end

let safe_div n d =
  (n + d - 1) / d
  
external get_array1_ptr 
  : ('a,'b,'c) Bigarray.Array1.t -> Int64.t = "get_bigarray_ptr"

external get_array2_ptr 
  : ('a,'b,'c) Bigarray.Array2.t -> Int64.t = "get_bigarray_ptr"

external get_array3_ptr 
  : ('a,'b,'c) Bigarray.Array3.t -> Int64.t = "get_bigarray_ptr"

