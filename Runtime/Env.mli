
val lookup : ID.t -> DataId.t Value.t

val set_binding : ID.t -> DataId.t Value.t -> unit    
val set_bindings : ID.t list -> DataId.t Value.t list -> unit 


type env = Value.t ID.Map.t  
val curr_env : env 
val push_env : unit -> unit
val pop_env : unit -> unit 

(* store mappings from abstract value to memory-space specific values *) 
type data_table = (DataId.t Value.t, Data.t) Hashtbl.t
val get_memspace_table : MemId.t -> data_table 
val register : Data.t -> DataId.t Value.t
 
val from_memspace : Data.t Value.t -> DataId.t Value.t
val to_memspace : DataId.t Value.t -> MemId.t -> Data.t Value.t

val dissociate : DataId.t Value.t -> MemId.t -> unit
