
class type ptr = object
  method addr : Int64.t 
  method free : unit
  
  method get_bool : int -> bool 
  method get_int32 : int -> Int32.t
  method get_float32 : int -> float 
  method get_float64 : int -> float  
end

module MemspaceRegistry = struct
  let register name = ??? 
end 

module type MEMSPACE = sig 
  type ptr
  
  val id : int 
  
  val addr : ptr -> Int64.t
  val alloc : int -> ptr  
  val free : ptr -> unit 
  
  val to_host : src:ptr -> dest:Int64.t -> nbytes:int -> unit
  val from_host : src:Int64.t -> dest:ptr -> nbytes:int -> unit   
  
  val get_bool : ?offset:int -> ptr -> bool
  val get_char : ?offset:int -> ptr -> char
  val get_int32 : ?offset:int -> ptr -> Int32.t
  val get_int64 : ?offset:int -> ptr -> Int64.t 
  val get_float32 : ?offset:int -> ptr -> float 
  val get_float64 : ?offset:int -> ptr -> float
end

module type DATA = sig 
  module M : MEMSPACE
   
  type array_desc = { 
    array_ptr : M.ptr; 
    shape_ptr : M.ptr; 
    strides_ptr : M.ptr; 
    rank : int;
    array_type : Type.t; 
    elt_type : Type.elt_type;    
  }
  
  
  type t = Array of array_desc | Scalar of Type.elt_type * M.ptr
end

module Data(M:MEMSPACE) = struct
  module M = M 
  type array_desc = { 
    array_ptr : ptr; 
    shape_ptr : ptr; 
    strides_ptr : ptr; 
    rank : int;
    array_type : Type.t; 
    elt_type : Type.elt_type;    
  }
  
  type t = Array of array_desc | Scalar of Type.elt_type * ptr  
end

(* Can we put Data(GpuMem).t and Data(HostMem).t in the same list? 
   Yes! It would have type "(module DATA) list"
  
   Is there a cleaner alternative encoding? 
*)