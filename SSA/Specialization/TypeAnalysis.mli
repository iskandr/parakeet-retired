open SSA
 
val type_analysis : 
      specializer : (value -> Signature.t -> fn) ->
      output_arity : (value->int)->  
      closureEnv : CollectPartialApps.closure_env -> 
      fn : fn -> 
      signature : Signature.t -> 
      (ID.t, Type.t) Hashtbl.t       
  
  