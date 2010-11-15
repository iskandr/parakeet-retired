

type t

val create : int -> t 

val add_host : t -> HostVal.host_val -> InterpVal.t 
val add_gpu : t -> GpuVal.gpu_val -> InterpVal.t 

val get_type : t -> InterpVal.t -> DynType.t
val get_shape : t -> InterpVal.t -> Shape.t  
 

val is_on_gpu : t -> InterpVal.t -> bool 
val is_on_host : t -> InterpVal.t -> bool 

val get_gpu : t -> InterpVal.t -> GpuVal.gpu_val 
val get_host : t -> InterpVal.t -> HostVal.host_val 

val free_gpu : t -> InterpVal.DataId.t -> unit 
val free_host : t -> InterpVal.DataId.t -> unit 

val slice : t -> InterpVal.t -> int -> InterpVal.t 

(* free identifier on both host and gpu *) 
val free : t -> InterpVal.DataId.t -> unit 

val free_all_gpu : t -> unit 
