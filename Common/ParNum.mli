type t = 
  | Bool of bool 
  | Char of char 
  | Int16 of int 
  | Int32 of Int32.t 
  | Int64 of Int64.t
  | Float32 of float
  | Float64 of float
  | Inf of Type.t
  | NegInf of Type.t 


val to_str : t -> string
val type_of_t : t -> Type.t

val coerce_int : int -> Type.t -> t 
val coerce_int32 :  Int32.t -> Type.t -> t 
val coerce_int64 : Int64.t -> Type.t -> t 
val coerce_float : float -> Type.t -> t 
val coerce : t -> Type.t -> t 

val to_int : t -> int
val to_int32 : t -> Int32.t 
val to_float : t -> float 

val is_zero : t -> bool 
val is_one : t -> bool 
val is_inf : t -> bool 
  
 