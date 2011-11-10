
val register_untyped_function : 
  name:string -> globals:string list -> args:string list -> AST.node -> FnId.t
  
val register_untyped_functions : 
  (string * string list * string list * AST.node) list -> unit
  
  
type ret_val = 
  | Success of Ptr.t Value.t (*  program succeeded, here's the result *) 
  | Pass (* I don't want to run this function *) 
  | Error of string (* I tried to run this function but failed *) 
  
val run_function : 
    FnId.t -> globals:Ptr.t Value.t list -> args:Ptr.t Value.t list -> ret_val