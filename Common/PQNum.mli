type num = 
  | Bool of bool 
  | Char of char 
  | UInt16 of int 
  | Int16 of int 
  | UInt32 of Uint64.t
  | Int32 of Int32.t 
  | UInt64 of Uint64.t   
  | Int64 of Int64.t
  | Float32 of float
  | Float64 of float
  | Inf of DynType.t
  | NegInf of DynType.t 


val num_to_str : num -> string
val type_of_num : num -> DynType.t

val coerce_int : int -> DynType.t -> num 
val coerce_int32 :  Int32.t -> DynType.t -> num 
val coerce_int64 : Int64.t -> DynType.t -> num 
val coerce_float : float -> DynType.t -> num 
val coerce_num : num -> DynType.t -> num 

val to_int : num -> int
val to_int32 : num -> Int32.t 
val to_float : num -> float 

val is_zero : num -> bool 
val is_one : num -> bool 
  
 