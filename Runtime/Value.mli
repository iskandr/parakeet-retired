(* 'a parameter can either be ArrayId.t when we don't care about what*)
(*  memory space data actually lives in or it can be Array.t when we do *) 

type 'a array_info = { 
  data : 'a; 
  array_type : Type.t; 
  elt_type : Type.elt_t;
  array_shape : Shape.t; 
  array_strides : int array;    
}  

type 'a t = 
  | Array of 'a array_info  
  | Scalar of ParNum.t
  | Explode of ParNum.t * Shape.t           (* scalar, shape *) 
  | Rotate of 'a t * int * int              (* array, dim, offset *) 
  | Shift of 'a t *  int * int * ParNum.t   (* array, dim, offset, default *) 
  | Slice of 'a t * int * int * int         (* array, dim, start, end *) 
  | Range of int * int                      (* start, stop *) 

val map : ('a -> 'b) -> 'a t -> 'b t

val to_str : ?array_to_str:('a array_info -> string) -> 'a t -> string 

val type_of : 'a t -> Type.t
val shape_of : 'a t -> Shape.t 

val to_num : 'a t -> ParNum.t
val to_bool : 'a t -> bool
val to_char : 'a t -> char 
val to_int32 : 'a t -> Int32.t 
val to_int64 : 'a t -> Int64.t 
val to_int : 'a t -> int
val to_float : 'a t -> float 

val of_num : ParNum.t -> 'a t 
val of_bool : bool -> 'a t
val of_char : char -> 'a t 
val of_int32 : Int32.t -> 'a t 
val of_int64 : Int64.t -> 'a t   
val of_int : int -> 'a t 
val of_float32 : float -> 'a t 
val of_float : float -> 'a t

val mk_array : 'a -> Type.elt_t -> Shape.t -> int array -> 'a t

val is_scalar : 'a t -> bool

val get_shape : 'a t -> Shape.t
val get_strides : 'a t -> int array

val extract : 'a t -> 'a option  
val collect_list : 'a t list -> 'a list  
