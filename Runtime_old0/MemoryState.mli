

type t

val create : unit -> t 

(* use these functions to
   create/destory environments 
   when entering/exiting functions 
*)
val enter_scope : t -> unit 
val exit_scope : ?escaping_values:InterpVal.t list -> t -> unit  

val enter_data_scope : t -> unit 
val exit_data_scope : ?escaping_values:InterpVal.t list -> t -> unit  
 
val lookup : t -> ID.t -> InterpVal.t

val set_binding : t -> ID.t -> InterpVal.t -> unit    
val set_bindings : t -> ID.t list -> InterpVal.t list -> unit 

val add_host : t -> HostVal.host_val -> InterpVal.t 
val add_gpu : t -> GpuVal.gpu_val -> InterpVal.t 

val get_type : t -> InterpVal.t -> Type.t
val get_shape : t -> InterpVal.t -> Shape.t  
 
val is_on_gpu : t -> InterpVal.t -> bool 
val is_on_host : t -> InterpVal.t -> bool 

val get_gpu : t -> InterpVal.t -> GpuVal.gpu_val 
val get_host : t -> InterpVal.t -> HostVal.host_val 
val get_scalar : t -> InterpVal.t -> ParNum.t

val slice_gpu_val : t -> GpuVal.gpu_val -> int -> GpuVal.gpu_val
val slice : t -> InterpVal.t -> int -> InterpVal.t 

val sizeof : t -> InterpVal.t -> int

val mk_gpu_vec : 
  t -> ?refcount:int -> ?nbytes:int -> Type.t -> Shape.t -> GpuVal.gpu_vec
  
val mk_host_vec : 
  t -> ?refcount:int-> ?nbytes:int -> Type.t -> Shape.t -> HostVal.host_array
   
val mk_gpu_val : t -> Type.t -> Shape.t -> GpuVal.gpu_val 

val flush_gpu : t -> unit    
val flush_gpu_to_host : t -> unit

(* UNMANAGED ALLOCATION! ONLY FOR USE WITH FLIP_DATA_LAYOUT! *)
val alloc_gpu : t -> int -> Cuda.GpuPtr.t 
