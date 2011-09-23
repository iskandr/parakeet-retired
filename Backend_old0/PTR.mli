

class type ptr = object
  method addr : Int64.t 
  method free : unit
  
  method get_bool : int -> bool 
  method get_int32 : int -> Int32.t
  method get_float32 : int -> float 
  method get_float64 : int -> float  
end

val alloc : int -> ptr 