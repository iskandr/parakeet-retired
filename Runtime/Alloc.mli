
val c_malloc : int -> Int64.t
val c_free : Int64.t -> unit 
val c_memcpy : Int64.t -> Int64.t -> int -> unit 

val get_array1_ptr : ('a,'b,'c) Bigarray.Array1.t -> Int64.t
val get_array2_ptr : ('a,'b,'c) Bigarray.Array2.t -> Int64.t
val get_array3_ptr : ('a,'b,'c) Bigarray.Array3.t -> Int64.t

val shape_to_gpu : Shape.t -> Cuda.GpuPtr.t * int 

val alloc_host_vec 
    : ?nbytes:int -> ?len:int -> DynType.t -> Shape.t -> HostVal.host_array

val alloc_gpu_vec 
    : ?nbytes:int -> ?len:int -> DynType.t -> Shape.t -> GpuVal.gpu_vec

val delete_host_vec : HostVal.host_array -> unit
val delete_host_val : HostVal.host_val -> unit

val delete_gpu_vec : GpuVal.gpu_vec -> unit    
val delete_gpu_val : GpuVal.gpu_val -> unit

val vec_to_gpu : HostVal.host_array -> GpuVal.gpu_vec 
val to_gpu :  HostVal.host_val -> GpuVal.gpu_val

val vec_from_gpu : GpuVal.gpu_vec -> HostVal.host_array  
val from_gpu : GpuVal.gpu_val -> HostVal.host_val


    
