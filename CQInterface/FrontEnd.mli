

val interpState : InterpState.t 

val register_untyped_function : 
  name:string -> globals:string list -> args:string list ->  AST.node -> FnId.t
  
val register_untyped_functions : 
  (string * string list * string list * AST.node) list -> unit
  
type ret_val = 
  | Success of HostVal.host_val (* gpu program succeeded, here's the result *) 
  | Pass (* I don't want to run this function *) 
  | Error of string (* I tried to run this function but failed *) 
  
val run_function : 
  FnId.t -> globals:HostVal.host_val list -> args:HostVal.host_val list -> 
    ret_val