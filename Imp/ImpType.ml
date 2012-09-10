(* pp: -parser o pa_macro.cmo *)
open Base

type elt_t = Type.elt_t

let elt_to_str : elt_t -> string  = Type.elt_to_str

type t =
  | ScalarT of elt_t
  | PtrT of elt_t * int option
  | TupleT of t array 
  | ArrayT of elt_t * int
  | ExplodeT of elt_t * int
  | RotateT of t
  | ShiftT of t
  | RangeT of elt_t
  | VectorT of elt_t * int (* type, width *)

let rec to_str = function
  | ScalarT elt_t -> elt_to_str elt_t
  | PtrT (t, len) ->
    Printf.sprintf "ptr(%s)%s"
      (elt_to_str t)
      (match len with Some k -> "[" ^ string_of_int k ^ "]" | None -> "")
  | TupleT ts -> 
    Printf.sprintf "tuple(%s)" (type_array_to_str ts) 
  | ArrayT (elt_t, r) ->
    Printf.sprintf "array%d<%s>" r (elt_to_str elt_t)
 
  | VectorT (elt_t, w) ->
    Printf.sprintf "vec%d(%s)" w (elt_to_str elt_t)
  | ShiftT t -> Printf.sprintf "shift(%s)" (to_str t)
  | RangeT t -> Printf.sprintf "range(%s)" (elt_to_str t)
  | RotateT t -> Printf.sprintf "rotate(%s)" (to_str t)
  | ExplodeT (t, n) -> Printf.sprintf "explode(%s, %d)" (elt_to_str t) n


and type_list_to_str ts = String.concat ", " (List.map to_str ts)
   
and type_array_to_str (ts : t array) = 
  String.concat ", " (Array.to_list (Array.map to_str ts))

let rec elt_type = function
  | RangeT t
  | ScalarT t
  | ExplodeT (t, _)
  | VectorT (t, _)
  | PtrT (t, _)
  | ArrayT (t, _) -> t
  | RotateT nested
  | ShiftT nested -> elt_type nested
  | TupleT ts -> 
    failwith $ Printf.sprintf 
      "Can't get elt_type of tuple(%s)" (type_array_to_str ts)

let is_scalar = function
  | ScalarT _ -> true
  | _ -> false

let is_int = function
  | ScalarT Type.BoolT
  | ScalarT Type.CharT
  | ScalarT Type.Int16T
  | ScalarT Type.Int32T
  | ScalarT Type.Int64T -> true
  | _ -> false

let is_float = function
  | ScalarT Type.Float32T
  | ScalarT Type.Float64T -> true
  | _ -> false

let is_vector = function
  | VectorT _ -> true
  | _ -> false


let is_array t = not (is_scalar t)

let rec rank = function
  | ScalarT _ -> 0
  | VectorT (_, _)
  | PtrT _ -> 1
  | ExplodeT (_, r)
  | ArrayT (_, r) -> r
  | ShiftT x -> rank x
  | _ -> failwith "Not implemented"

let bool_t = ScalarT Type.BoolT
let char_t = ScalarT Type.CharT
let int16_t = ScalarT Type.Int16T
let int32_t = ScalarT Type.Int32T
let int64_t = ScalarT Type.Int64T
let float32_t = ScalarT Type.Float32T
let float64_t = ScalarT Type.Float64T

let common_type t1 t2 = match t1, t2 with
  | ScalarT t1', ScalarT t2' -> ScalarT (Type.common_elt_type t1' t2')
  | _ -> if t1 = t2 then t1 else
         failwith ("Can't unify imp types " ^ (to_str t1) ^ " and " ^
                  (to_str t2))

let rec combine_type_list = function
  | [] -> failwith "Can't combine empty type list"
  | [t] -> t
  | t::ts -> common_type t (combine_type_list ts)


let peel ?(num_axes=1) = function
  | ArrayT (eltT, r) ->
    let diff = r - num_axes in
    if diff = 0 then ScalarT eltT
    else if diff > 0 then ArrayT (eltT, diff)
    else failwith "[ImpType.peel] Too many axes"
  | VectorT (eltT, _)
  | PtrT (eltT,_) when num_axes = 1 -> ScalarT eltT
  | ScalarT eltT -> ScalarT eltT
  | _ -> failwith "Not implemented"


let rec type_of_value = function
  | Value.Scalar n -> ScalarT (ParNum.type_of n)
  | Value.Array a -> ArrayT (a.Value.elt_type, Shape.rank a.Value.array_shape)
  | Value.Shift (nested, _, _, _) -> ShiftT (type_of_value nested)
  | Value.Explode (n, s) ->
      ExplodeT (ParNum.type_of n, Shape.rank s)
  | Value.Rotate (x, _, _) -> RotateT (type_of_value x)
  | Value.FixDim(a, _, _) -> peel (type_of_value a)
  | Value.Tuple elts -> TupleT (Array.map type_of_value elts)
  | other ->
    failwith $
      Printf.sprintf "[ImpType] Not implemented for %s"
      (Value.to_str other)

let type_of_copy t =
  if is_scalar t then t
  else
    let r = rank t in
    let eltT = elt_type t in
    ArrayT(eltT, r)
