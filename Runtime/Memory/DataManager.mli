(* store mappings from abstract value to memory-space specific values *) 
type data_table = (DataId.t, Ptr.t) Hashtbl.t

val get_memspace_table : MemId.t -> data_table 
val register_ptr : Ptr.t -> DataId.t 

val id_to_ptr_option : DataId.t -> MemId.t -> Ptr.t option 
val id_on_memspace : DataId.t -> MemId.t -> bool 
val id_to_ptr : DataId.t -> MemId.t -> Ptr.t 
 
val from_memspace : Ptr.t Value.t -> DataId.t Value.t
val to_memspace : MemId.t ->  DataId.t Value.t -> Ptr.t Value.t
