type t = 
  | Bool of bool 
  | Char of char 
  | Int16 of int 
  | Int32 of Int32.t 
  | Int64 of Int64.t
  | Float32 of float
  | Float64 of float
  | Inf of Type.elt_t
  | NegInf of Type.elt_t 

val is_bool : t -> bool
val is_char : t -> bool
val is_int16 : t -> bool
val is_int32 : t -> bool
val is_int64 : t -> bool
val is_float32 : t -> bool
val is_float64 : t -> bool
val is_pos_inf : t -> bool
val is_neg_inf : t -> bool

val is_int : t -> bool
val is_float : t -> bool
val is_inf : t -> bool 

val to_str : t -> string
val type_of : t -> Type.elt_t

val coerce_bool : bool -> Type.elt_t -> t 
val coerce_char : char -> Type.elt_t -> t
val coerce_int : int -> Type.elt_t -> t 
val coerce_int32 :  Int32.t -> Type.elt_t -> t 
val coerce_int64 : Int64.t -> Type.elt_t -> t 
val coerce_float : float -> Type.elt_t -> t 
val coerce : t -> Type.elt_t -> t 

val of_bool : bool -> t 
val of_char : char -> t
val of_int : int -> t 
val of_int32 : Int32.t -> t
val of_int64 : Int64.t -> t 
val of_float32 : float -> t 
val of_float : float -> t 

val to_bool : t -> bool
val to_char : t -> char 
val to_int : t -> int
val to_int32 : t -> Int32.t 
val to_int64 : t -> Int64.t 
val to_float : t -> float 

val is_zero : t -> bool 
val is_one : t -> bool 
val is_inf : t -> bool 