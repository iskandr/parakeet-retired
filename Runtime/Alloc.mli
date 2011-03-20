
val c_malloc : int -> Int64.t
val c_free : Int64.t -> unit 
val c_memcpy : Int64.t -> Int64.t -> int -> unit 

val alloc_host_vec 
    : ?nbytes:int -> ?len:int -> DynType.t -> Shape.t -> HostVal.host_val 

val alloc_gpu_vec 
    : ?nbytes:int -> ?len:int -> DynType.t -> Shape.t -> GpuVal.gpu_val

val delete_host_val : HostVal.host_val -> unit   
val delete_gpu_val : GpuVal.gpu_val -> unit

val gpu_slice : GpuVal.gpu_val -> int -> GpuVal.gpu_val
val host_slice : HostVal.host_val -> int -> HostVal.host_val  

val to_gpu : ?prealloc:Cuda.GpuPtr.t -> HostVal.host_val -> GpuVal.gpu_val 
val from_gpu :?prealloc:Cuda.HostPtr.t -> GpuVal.gpu_val -> HostVal.host_val

val shape_to_gpu : Shape.t -> GpuVal.gpu_val
    
