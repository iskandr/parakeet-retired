(* abstract interface for all pointer modules *) 
type t 

val of_int64 : Int64.t -> t
val to_int64 : t -> Int64.t 

val offset : t -> int -> t 

val malloc : int -> t
val free : t -> unit 
val memcpy : t -> t -> int -> unit  

val get_bool : t -> ?offet:int -> bool
val set_bool : t -> ?offset:int -> bool -> unit 

val get_char : t -> ?offset:int -> char
val set_char : t -> ?offset:int -> char -> unit 
 
val get_int32 : t -> ?offset:int -> Int32.t
val set_int32 : t -> ?offset:int -> Int32.t -> unit
 
val get_int64 : t -> ?offset:int -> Int64.t
val set_int64 : t -> ?offset:int -> Int64.t -> unit 

val get_float32 : t -> ?offset:int -> float
val set_float32 : t -> ?offset:int -> float -> unit

val get_float64 : t -> ?offset:int -> float
val set_float64 : t -> ?offset:int -> float -> unit 