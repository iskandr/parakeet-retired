

type adverb_impl = 
  MemoryState.mem_state -> FnTable.t -> SSA.fundef -> 
    GpuVal.gpu_val list -> DynType.t list -> GpuVal.gpu_val list

type simple_array_op_impl =
  MemoryState.mem_state -> FnTable.t -> 
    GpuVal.gpu_val list -> DynType.t list -> GpuVal.gpu_val list
  

val run_map : adverb_impl 
    
val run_reduce : adverb_impl 

val run_reduce : adverb_impl 
        
val eval_array_op :  
     MemoryState.mem_state -> FnTable.t ->  Prim.array_op -> 
        InterpVal.t list -> DynType.t list -> GpuVal.gpu_val list 

(*val init : unit -> unit 
val shutdown : unit -> unit

*)
       