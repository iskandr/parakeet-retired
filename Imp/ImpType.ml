
type elt_t = Type.elt_t 
type t =
	| ScalarT of elt_t  
	| ArrayT of elt_t * int
	| ShiftT of t 

let get_elt_type = function 
	| ScalarT t -> t 
	| _ -> assert false 

let rec rank = function 
	| ScalarT _ -> 0 
	| ArrayT (_, r) -> r
	| ShiftT x -> rank x 