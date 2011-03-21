

type t

val create : int -> t 

(* use these functions to
   create/destory environments 
   when entering/exiting functions 
*)
val push_env : t -> unit 
val pop_env : t -> unit

val get_curr_env : t -> InterpVal.t ID.Map.t
val env_lookup : t -> ID.t -> InterpVal.t

val env_set : t -> ID.t -> InterpVal.t -> unit    
val env_set_multiple : t -> ID.t list -> InterpVal.t list -> unit 

val add_host : t -> HostVal.host_val -> InterpVal.t 
val add_gpu : t -> GpuVal.gpu_val -> InterpVal.t 

val get_type : t -> InterpVal.t -> DynType.t
val get_shape : t -> InterpVal.t -> Shape.t  
 
val is_on_gpu : t -> InterpVal.t -> bool 
val is_on_host : t -> InterpVal.t -> bool 

val get_gpu : t -> InterpVal.t -> GpuVal.gpu_val 
val get_host : t -> InterpVal.t -> HostVal.host_val 
val get_scalar : t -> InterpVal.t -> PQNum.num 

val slice : t -> InterpVal.t -> int -> InterpVal.t 

val free_gpu : t -> InterpVal.DataId.t -> unit 
val free_host : t -> InterpVal.DataId.t -> unit  
val free_all_gpu : t -> unit
val free : t -> InterpVal.DataId.t -> unit

val sizeof : t -> InterpVal.t -> int

val mk_gpu_vec 
    : t -> ?freeze:bool -> DynType.t -> Shape.t -> GpuVal.gpu_vec

val mk_gpu_val : t -> ?freeze:bool -> DynType.t -> Shape.t -> GpuVal.gpu_val 

val unfreeze_gpu_vec : t -> GpuVal.gpu_vec -> unit 
val unfreeze_gpu_vecs : t -> GpuVal.gpu_vec list -> unit  
  