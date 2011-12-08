


type elt_t = Type.elt_t 

type t =
	| ScalarT of elt_t  
	| ArrayT of elt_t * int
	| ShiftT of t
  | FixedDimT of t 
  | Tuple of t array 

val to_str : t -> string 

val get_elt_type : t -> elt_t
val is_scalar : t -> bool 
val rank : t -> int 

val bool_t : t 
val char_t : t 
val int16_t : t 
val int32_t : t
val int64_t : t 
val float32_t : t
val float64_t : t 
