(* 'a parameter can either be ArrayId.t when we don't care about what*)
(*  memory space data actually lives in or it can be Array.t when we do *) 

 

type 'a t = 
  | Array of 'a * Type.elt_t * Shape.t  
  | Scalar of ParNum.t
  | Nested of ('a t) array 
  | Explode of ParNum.t * Shape.t           (* scalar, shape *) 
  | Rotate of 'a t * int * int                (* array, dim, offset *) 
  | Shift of 'a t * int * int * ParNum.t     (* array, dim, offset, default *) 
  | Slice of 'a t * int * int * int         (* array, dim, start, end *) 
  | Range of int * int                      (* start, stop *) 

val map : ('a -> 'b) -> 'a t -> 'b t
 
val to_str : ?array_to_str:('a -> string) -> 'a t -> string 

val to_int : 'a t -> int 
val to_bool : 'a t -> bool  
val to_num : 'a t -> ParNum.t 

val of_bool : bool -> 'a t  
val of_int : int -> 'a t 
val of_float : float -> 'a t 

val get_type : 'a t -> Type.t
val get_shape : 'a t -> Shape.t 

