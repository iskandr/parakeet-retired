
val register_untyped_function :
      name:string -> 
        globals:string list -> 
          positional_args: string list -> 
            default_arg_names : string list -> 
              default_arg_values : AST.node list -> 
                body: AST.node -> 
                  FnId.t

type ret_val =
  | Success of Ptr.t Value.t list (*  program succeeded, here are the results *)
  | Error of string (* I tried to run this function but failed *)
  | Pass (* I don't want to run this function *)


type value = Ptr.t Value.t 
type values = value list 

val run_function :
  untyped_id:FnId.t -> 
    globals:values ->  
      actuals : value Args.actual_args -> 
            ret_val

val run_adverb :  
  adverb:Prim.adverb -> 
    fn_id: FnId.t ->
      fixed : value Args.actual_args ->
        combine_id : FnId.t option ->
          combine_fixed : value Args.actual_args ->   
            init : values -> 
              axes : int list option ->   
                arrays : value Args.actual_args ->
                  ret_val 
                
