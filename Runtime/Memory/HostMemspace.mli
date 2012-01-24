exception HostOutOfMemory
val malloc : int -> Int64.t 
   
val free : Int64.t -> unit 
val memcpy : Int64.t -> Int64.t -> int -> unit    
    
val get_array1_ptr : ('a,'b,'c) Bigarray.Array1.t -> Int64.t 
val get_array2_ptr : ('a,'b,'c) Bigarray.Array2.t -> Int64.t 
val get_array3_ptr : ('a,'b,'c) Bigarray.Array3.t -> Int64.t

val get_int32 : Int64.t -> int -> Int32.t
val set_int32 : Int64.t -> int -> Int32.t -> unit 

val get_int64 : Int64.t -> int -> Int64.t
val set_int64 : Int64.t -> int -> Int64.t -> unit

val get_float32 : Int64.t -> int -> float 
val set_float32 : Int64.t -> int -> float -> unit

val get_float64 : Int64.t -> int -> float 
val set_float64 : Int64.t -> int -> float -> unit 

val get_char : Int64.t -> int -> char 
val set_char : Int64.t -> int -> char -> unit 

val get_bool : Int64.t -> int -> bool   
val set_bool : Int64.t -> int -> bool -> unit  

val deref_scalar : Int64.t -> Type.elt_t -> ParNum.t 

val id : MemId.t 

val mk_host_ptr : Int64.t -> int -> Ptr.t

