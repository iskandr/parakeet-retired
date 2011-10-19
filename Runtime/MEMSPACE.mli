class type ptr = object 
  method addr : Int64.t 
  method free : unit 
  
  method memspace_id : MemId.t 
  
  method get_bool : int -> bool
  method get_char : int -> char
  method get_int32 : int -> Int32.t 
  method get_int64 : int -> Int64.t 
  method get_float32 : int -> float
  method get_float64 : int -> float
end 

val alloc : int -> Ptr.t
val of_int64 : Int64.t -> Ptr.t 
val memspace_id : MemId.t  
