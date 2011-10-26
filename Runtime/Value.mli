(* 'a parameter can either be ArrayId.t when we don't care about what*)
(*  memory space data actually lives in or it can be Array.t when we do *) 

type 'a t = 
  | Scalar of ParNum.t
  | Array of 'a
  | Nested of ('a t) array 
  | Explode of ParNum.t * Shape.t 
  | Rotate of 'a t * int * int  
  | Shift of 'a t * int * int * ParNum.t
  | Slice of 'a t * int * int * int 
  | Range of int * int 


val map : ('a -> 'b) -> 'a t -> 'b t
 
val to_str : ?array_to_str:('a -> string) -> t -> string 

val to_int : t -> int 
val to_bool : t -> bool  
val to_num : t -> ParNum.t 

val of_bool : bool -> t  
val of_int : int -> t 
val of_float : float -> t 

