


open Base 

type env = DataId.t Value.t ID.Map.t  
val curr_env : unit -> env 

val push_env : unit -> unit
val pop_env : unit -> unit 

val lookup : ID.t -> DataId.t Value.t

val set_binding : ID.t -> DataId.t Value.t -> unit    
val set_bindings : ID.t list -> DataId.t Value.t list -> unit 

val active_values : unit -> DataId.t Value.t list  
val active_ptrs : MemId.t -> Ptr.t list
val active_addrs : MemId.t -> Int64.t list 
val active_addr_set : MemId.t -> Int64.Set.t  