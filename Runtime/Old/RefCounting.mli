

val getref : DataId.t -> int
val setref : DataId.t -> int -> unit 
val remove : DataId.t -> unit

val get_dead_ids : unit -> DataId.t list
val iter_dead_ids : (DataId.t -> unit) -> unit  

val incref_data_id : DataId.t -> unit 
val decref_data_id : DataId.t -> unit 

val incref : Value.t -> unit 
val decref : Value.t -> unit 
