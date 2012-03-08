type elt_t = Type.elt_t

type t =
  | ScalarT of elt_t
  | PtrT of elt_t * int option
  | ArrayT of elt_t * int
  | ExplodeT of elt_t * int
  | RotateT of t
  | ShiftT of t
  | RangeT of elt_t
  | VectorT of elt_t * int

val to_str : t -> string
val type_list_to_str : t list -> string

val elt_type : t -> elt_t
val is_scalar : t -> bool
val is_int : t -> bool
val is_float : t -> bool
val is_vector : t -> bool
val is_array : t -> bool
val rank : t -> int

val bool_t : t
val char_t : t
val int16_t : t
val int32_t : t
val int64_t : t
val float32_t : t
val float64_t : t

val common_type : t -> t -> t
val combine_type_list : t list -> t

val type_of_value : 'a Value.t -> t

val peel : ?num_axes:int -> t -> t

val type_of_copy : t -> t
