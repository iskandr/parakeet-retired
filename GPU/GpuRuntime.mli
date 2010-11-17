


val implements_array_op : Prim.array_op -> bool 
                
val eval_array_op :  
     MemoryState.t -> FnTable.t ->  Prim.array_op -> 
        InterpVal.t list -> DynType.t list -> InterpVal.t list 

(*val init : unit -> unit 
val shutdown : unit -> unit

*)
       