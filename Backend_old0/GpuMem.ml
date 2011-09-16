



class gpu_ptr addr = object 

  method addr : Int64.t = addr
  method free : unit = () (* call cudafree *) 
  
  method get_int32 offset : Int32.t = Int32.zero (* deref addr *)
  method get_float32 offset : float = 0.0 (* deref addr *) 

end 

let alloc nbytes = 
  let (ptr:Int64.t) = cuda_malloc nbytes in 
  