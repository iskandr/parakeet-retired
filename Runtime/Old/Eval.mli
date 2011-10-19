

(* evaluates a function applied to a set of arguments on the host *) 
val eval
  : FnTable.t -> SSA.fundef ->  HostVal.host_val list -> HostVal.host_val list
