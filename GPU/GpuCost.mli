
val map : 
      memState:MemoryState.t -> fnTable:FnTable.t -> fn:SSA.fundef -> 
        closureArgs:InterpVal.t list -> dataArgs:InterpVal.t list -> int  
    
val reduce : 
      memState:MemoryState.t -> fnTable:FnTable.t -> init:SSA.fundef ->
        initClosureArgs:InterpVal.t list -> fn:SSA.fundef -> 
          closureArgs:InterpVal.t list -> args:InterpVal.t list -> int  

val array_op : MemoryState.t -> Prim.array_op -> InterpVal.t list -> int           
