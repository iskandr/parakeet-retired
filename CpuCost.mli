

val map 
   : MemoryState.t -> InterpVal.t list -> InterpVal.t list -> SSA.fundef -> int  
    
val array_op : MemoryState.t -> Prim.array_op -> InterpVal.t list -> int 
