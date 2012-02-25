(* pp: -parser o pa_macro.cmo *)

type elt_t = Type.elt_t
type t =
  | ArrayT of elt_t * int
  | ScalarT of elt_t
  | ExplodeT of elt_t * int
  | RotateT of t
  | ShiftT of t
  | SliceT of t
  | RangeT of elt_t
  | VecSliceT of elt_t * int (* type, width *)

let rec to_str = function
	| ScalarT elt_t -> Type.elt_to_str elt_t
	| ArrayT (elt_t, r) ->
    Printf.sprintf "array%d<%s>" r (Type.elt_to_str elt_t)
	| ShiftT t -> Printf.sprintf "shift(%s)" (to_str t)
  | _ -> failwith "Not implemented"

let type_list_to_str ts = String.concat ", " (List.map to_str ts)

let rec elt_type = function
  | RangeT t
	| ScalarT t
  | ExplodeT (t, _)
  | VecSliceT (t, _)
  | ArrayT (t, _) -> t
  | RotateT nested
  | SliceT nested
  | ShiftT nested -> elt_type nested

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

let is_array = function
  | ArrayT _ -> true
  | _ -> false

let rec rank = function
	| ScalarT _ -> 0
  | ExplodeT (_, r)
  | VecSliceT (_, r)
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

let rec type_of_value = function
  | Value.Scalar n -> ScalarT (ParNum.type_of n)
  | Value.Array a -> ArrayT (a.Value.elt_type, Shape.rank a.Value.array_shape)
  | Value.Shift (nested, _, _, _) -> ShiftT (type_of_value nested)

let peel ?(num_axes=1) = function
  | ArrayT (eltT, r) ->
    let diff = r - num_axes in
    if diff = 0 then ScalarT eltT
    else if diff > 0 then ArrayT (eltT, diff)
    else failwith "[ImpType.peel] Too many axes"
  | ScalarT eltT -> ScalarT eltT
  | _ -> failwith "Not implemented"
