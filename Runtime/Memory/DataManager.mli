(* store mappings from abstract value to memory-space specific values *) 
type data_table = (DataId.t Value.t, Data.t) Hashtbl.t

val get_memspace_table : MemId.t -> data_table 
val register : Data.t -> DataId.t Value.t
 
val from_memspace : Data.t Value.t -> DataId.t Value.t
val to_memspace : MemId.t ->  DataId.t Value.t -> Data.t Value.t

val dissociate : DataId.t Value.t -> MemId.t -> unit

