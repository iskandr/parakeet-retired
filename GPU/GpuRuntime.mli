

type adverb_impl = 
  MemoryState.t -> FnTable.t -> SSA.fundef -> 
    GpuVal.gpu_val list -> DynType.t list -> GpuVal.gpu_val list

type simple_array_op_impl =
  MemoryState.t -> FnTable.t -> 
    GpuVal.gpu_val list -> DynType.t list -> GpuVal.gpu_val list
  

val run_map : adverb_impl 
    
val run_reduce : adverb_impl 

val run_reduce : adverb_impl 
        
val eval_array_op :  
     MemoryState.t -> FnTable.t ->  Prim.array_op -> 
        InterpVal.t list -> DynType.t list -> InterpVal.t list 

(*val init : unit -> unit 
val shutdown : unit -> unit

*)
       