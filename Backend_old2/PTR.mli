type t 
val alloc : int -> t
val free : t -> unit

val of_int64 : int -> t 
val to_int64 : t ->  Int64.t 
  
val memcpy_to_host : src:t -> dest:HostPtr.t -> nbytes:int -> unit 
val memcpy_from_host : src:HostPtr.t -> dest:t -> nbytes:int -> unit

val get_bool : t -> int -> bool
val get_char : t -> int -> char
val get_int32 : t -> int -> Int32.t 
val get_int64 : t -> int -> Int64.t 
val get_float32 : t -> int -> float
val get_float64 : int -> float

val memspace_id : MemspaceRegistry.id 
