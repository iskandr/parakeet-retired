

(* use these functions to
   create/destory environments 
   when entering/exiting functions 
*)
val enter_scope : unit -> unit 
val exit_scope : Value.t list -> unit  

val enter_data_scope : unit -> unit 
val exit_data_scope : Value.t list -> unit  
 
val lookup : ID.t -> Value.t

val set_binding : ID.t -> Value.t -> unit    
val set_bindings : ID.t list -> Value.t list -> unit 


(* TODO: get rid of host/gpu distinction, generalize to arbitrary *)
(* number of memory spaces *) 
val add_host : HostVal.host_val -> Value.t 
val add_gpu : GpuVal.gpu_val -> Value.t 

val get_type : Value.t -> Type.t
val get_shape : Value.t -> Shape.t  
 
val is_on_gpu : Value.t -> bool 
val is_on_host : Value.t -> bool 

val get_gpu : Value.t -> GpuVal.gpu_val 
val get_host : Value.t -> HostVal.host_val 
val get_scalar : Value.t -> ParNum.t

val slice_gpu_val : GpuVal.gpu_val -> int -> GpuVal.gpu_val
val slice : Value.t -> int -> Value.t 

val sizeof : Value.t -> int

val mk_gpu_vec : ?refcount:int -> ?nbytes:int -> Type.t -> Shape.t -> GpuVal.gpu_vec
val mk_host_vec : ?refcount:int-> ?nbytes:int -> Type.t -> Shape.t -> HostVal.host_array
val mk_gpu_val : Type.t -> Shape.t -> GpuVal.gpu_val 

val flush_gpu : unit -> unit    
val flush_gpu_to_host : unit -> unit

