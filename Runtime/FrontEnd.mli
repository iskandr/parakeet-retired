



val register_untyped_function : 
      name:string -> args:string list -> body:AST.node -> FnId.t
      
val run_function : 
      FnId.t -> globals:HostVal.host_val list -> args:HostVal.host_val list -> HostVal.host_val list  
         