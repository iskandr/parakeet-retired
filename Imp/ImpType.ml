
type elt_t = Type.elt_t
type t =
    | ScalarT of elt_t
    | ArrayT of elt_t * int
    | ShiftT of t
    | FixedDimT of t
    | Tuple of t array

let rec to_str = function
	| ScalarT elt_t -> Type.elt_to_str elt_t
	| ArrayT (elt_t, r) ->
      Printf.sprintf "array(%s, %d)" (Type.elt_to_str elt_t) r
	| ShiftT t -> Printf.sprintf "shift(%s)" (to_str t)

let get_elt_type = function
	| ScalarT t -> t
	| _ -> assert false

let is_scalar = function
	| ScalarT _ -> true
	| _ -> false

let rec rank = function
	| ScalarT _ -> 0
	| ArrayT (_, r) -> r
	| ShiftT x -> rank x

let bool_t = ScalarT Type.BoolT
let char_t = ScalarT Type.CharT
let int16_t = ScalarT Type.Int16T
let int32_t = ScalarT Type.Int32T
let int64_t = ScalarT Type.Int64T
let float32_t = ScalarT Type.Float32T
let float64_t = ScalarT Type.Float64T
