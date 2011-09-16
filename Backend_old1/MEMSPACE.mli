class ptr : object 
  method addr : Int64.t 
  method free : unit 
  
  method memcpy_to_host : addr:Int64.t -> nbytes:int -> unit 
  method memcpy_from_host : addr:Int64.t -> nbytes:int -> unit 
  
  method get_bool : int -> bool
  method get_char : int -> char
  method get_int32 : int -> Int32.t 
  method get_int64 : int -> Int64.t 
  method get_float32 : int -> float
  method get_float64 : int -> float
end 

val alloc : int -> ptr 
val of_int64 : int -> ptr 
val memspace_id : MemspaceRegistry.id 
