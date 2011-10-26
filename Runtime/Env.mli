

(* push or pop the current set of active DataId's without creating *)
(* a new environment *) 
val push_data_scope : unit -> unit
val pop_data_scope : Value.t list -> unit 

(* create both a new environment and a new set of active data *) 
val enter_scope : unit -> unit 
val exit_scope : Value.t list -> unit  

val lookup : ID.t -> Value.t

val set_binding : ID.t -> Value.t -> unit    
val set_bindings : ID.t list -> Value.t list -> unit 

