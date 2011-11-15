


type elt_t = Type.elt_t 

type t =
	| ScalarT of elt_t  
	| ArrayT of elt_t * int
	| ShiftT of t


val get_elt_type : t -> elt_t
val rank : t -> int 
