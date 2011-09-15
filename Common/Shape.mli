type t

(* create a new shape of the given length *) 
val create : int -> t 

(* what's the nth value of the shape? *) 
val get : t -> int -> int 
  
(* set the nth value in the shape *) 
val set : t -> int -> int -> unit 

val to_str : t -> string 

val scalar_shape : t 

val of_list : int list -> t 

val nelts : t -> int 

val rank : t -> int 

(* how much space does the shape vector take up? *)
val nbytes : t -> int 

val eq : t -> t -> bool 

val is_subshape : t -> t -> bool 

val max_shape : t -> t -> t option 

val max_shape_list : t list -> t option 


val append : t -> t -> t   

val append_dim : int -> t -> t 

val append_dims : int list -> t -> t 

val peel : ?axes:int list -> t -> t

(* return max outer dim, and nested shapes *)
val split_nested_shapes : t list -> int * t list 

(* remove dims specified int the list of dims *) 
val slice_shape : t -> int list -> t 


open Bigarray 
val to_c_array : t -> (int32, int32_elt, c_layout) Array1.t 
val of_c_array : (int32, int32_elt, c_layout) Array1.t -> t 

 