open Base
open Printf

type elt_t =
  | BoolT
  | CharT
  | Int16T
  | Int32T
  | Int64T
  | Float32T
  | Float64T

type t  =
  | ScalarT of elt_t
  | ArrayT of elt_t * int
  | BottomT (* bottom of type lattice for vars whose type is not determined *)
  | AnyT (* top of type lattice, for variables whose type is overdetermined *)

let bool = ScalarT BoolT
let char = ScalarT CharT
let int16 = ScalarT Int16T
let int32 = ScalarT Int32T
let int64 = ScalarT Int64T
let float32 = ScalarT Float32T
let float64 = ScalarT Float64T

let is_int16 = function ScalarT Int16T -> true | _ -> false
let is_int32 = function ScalarT Int32T -> true | _ -> false
let is_int64 = function ScalarT Int64T -> true | _ -> false
let is_float32 = function ScalarT Float32T -> true | _ -> false
let is_float64 = function ScalarT Float64T -> true | _ -> false
let is_char = function ScalarT CharT -> true | _ -> false

let is_bool = function ScalarT BoolT -> true | _ -> false

let elt_is_int = function
  | BoolT
  | CharT
  | Int16T
  | Int32T
  | Int64T -> true
  | _ -> false

let elt_is_float = function
  | Float32T
  | Float64T -> true
  | _ -> false

let elt_is_number t = elt_is_int t || elt_is_float t

let is_int = function
  | ScalarT t -> elt_is_int t
  | _ -> false

let is_float = function
  | ScalarT t -> elt_is_float t
  | _ -> false

let is_number t = is_int t || is_float t

let is_scalar = function
  | ScalarT _ -> true
  | _ -> false

let is_compound t = not (is_scalar t)

let is_array = function
    | ArrayT _ -> true
    | _ -> false

let is_num_or_array = function
  | ScalarT _
  | ArrayT _  -> true
  | _ -> false

let is_numarray = function
    |  ArrayT (t, _) when elt_is_number t -> true
    | _ -> false

let elt_to_str = function
  | BoolT -> "bool"
  | CharT -> "char"
  | Int16T -> "int16"
  | Int32T -> "int32"
  | Int64T -> "int64"
  | Float32T -> "float32"
  | Float64T -> "float64"

let elt_to_short_str = function
  | BoolT -> "b"
  | CharT -> "c"
  | Int16T -> "s"
  | Int32T -> ""
  | Int64T -> "L"
  | Float32T -> "f"
  | Float64T -> ""

let to_str = function
  | BottomT -> "bottom"
  | AnyT -> "any"
  | ScalarT t -> (elt_to_str t)
  | ArrayT (eltT, d) -> Printf.sprintf "array%d<%s>" d (elt_to_str eltT)

let type_list_to_str ?(sep=", ") ts = String.concat sep (List.map to_str ts)
let type_array_to_str ts = type_list_to_str (Array.to_list ts)

let sizeof = function
  | CharT
  | BoolT -> 1
  | Int16T -> 2
  | Int32T
  | Float32T -> 4
  | Int64T
  | Float64T -> 8

let mk_array_type elt_t rank =
  if rank <> 0 then ArrayT(elt_t, rank)
  else ScalarT elt_t

let elt_type = function
  | ArrayT (t, _) -> t
  | ScalarT t -> t
  | t -> failwith ("Can't get elt type of " ^ (to_str t))

let rec fill_elt_type t e = match t with
  | ArrayT (_, r) -> ArrayT(e, r)
  | ScalarT _ -> ScalarT e
  | other -> other

(* reduce the dimensionality of an array by 1 *)
(* TODO: ACTUALLY USE AXES! *)
let peel ?(num_axes=1) = function
  | ArrayT(eltT, d) ->
        if d <= num_axes then ScalarT eltT
        else ArrayT(eltT, d-num_axes)
  | ScalarT t -> ScalarT t
  | t -> failwith
     (Printf.sprintf
        "[peel] expected array or scalar, got : %s"
        (to_str t)
     )

let is_scalar_subtype s1 s2 =
    (s1 = s2) ||
    (sizeof s1 < sizeof s2) ||
    (elt_is_int s1 && elt_is_float s2)

(* how deep is the nesting of vectors in a given type *)
let rank = function
  | ArrayT (_, d) -> d
  | _ -> 0

(*
  VecT IntT and VecT FloatT has equivalent structure, whereas
  VecT(VecT Int) and VecT IntT don't
*)
let equiv_type_structure t1 t2 = (rank t1 = rank t2)

(* VecT IntT is a structure subtype of VecT (VecT IntT) *)
(* BUT: VecT IntT is *not* a structure subtype of IntT *)
let rec is_structure_subtype t1 t2 = (rank t1 <= rank t2)

(*
  If t1 is a subtype of t2, returns how nested within t2 is t1.
  Otherwise returns None.
*)
let rec relative_rank t1 t2 =
  let diff = rank t1 - rank t2 in
  if diff >= 0 then Some diff
  else None

let common_elt_type t1 t2 =
  if t1 = t2 then t1
  else if is_scalar_subtype t1 t2 then t2
  else t1

let rec common_elt_type_list = function
  | [] -> BoolT
  | t::ts -> common_elt_type t (common_elt_type_list ts)

let common_type t1 t2  =
	if t1 = t2 then t1
  else match t1, t2 with
    | ScalarT s1, ScalarT s2 ->
      if is_scalar_subtype s1 s2 then t2
      else if is_scalar_subtype s2 s1 then t1
      else AnyT
    | ArrayT (s2, d), ScalarT s1
    | ScalarT s1, ArrayT(s2, d) ->
      if is_scalar_subtype s1 s2 then ArrayT(s2,d) else AnyT
    | ArrayT(s1, d1), ArrayT(s2, d2) ->
      if is_scalar_subtype s1 s2 && d1 <= d2 then t2
      else if is_scalar_subtype s2 s1 && d2 <= d1 then t1
      else AnyT
    | BottomT, _ -> t2
    | _, BottomT -> t1
    | _ -> AnyT

let combine_type_array arr =
  if Array.length arr < 1 then AnyT
  else
     let base_t = arr.(0) in
     Array.fold_left common_type base_t arr

let combine_type_list = function
  | [] -> AnyT
  | t::ts -> List.fold_left common_type t ts

let replace_elt_type t s = match t with
  | ArrayT (_, d) -> ArrayT(s,d)
  | ScalarT _ -> ScalarT s
  | _ -> t

let increase_rank r = function
  | ArrayT (elt_t, old_r) -> ArrayT (elt_t, old_r + r)
  | ScalarT elt_t -> ArrayT (elt_t, r)
  | other -> failwith ("[Type] Can't increase rank of type " ^ (to_str other))

let increase_ranks r ts = List.map (increase_rank r) ts

let maximal_type types =
  let rec aux highestRank currT = function
    | [] -> currT
    | t::ts ->
      let r = rank t in
      if r >= highestRank then aux r t ts
      else aux highestRank currT ts
  in aux 0 BottomT types
