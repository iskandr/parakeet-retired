


(* given a memstate, fntable, function definition, 
   closure arguments, direct map arguments and output types, 
   return gpu values 
*) 
val run_map : 
  MemoryState.t -> FnTable.t -> SSA.fundef -> 
    GpuVal.gpu_val list -> GpuVal.gpu_val list -> 
    DynType.t list -> GpuVal.gpu_val list     

val implements_array_op : Prim.array_op -> bool 
                
val eval_array_op :  
     MemoryState.t -> FnTable.t ->  Prim.array_op -> 
        InterpVal.t list -> DynType.t list -> InterpVal.t list 
