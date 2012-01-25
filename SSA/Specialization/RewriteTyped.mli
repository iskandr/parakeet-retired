open SSA

val rewrite_typed : 
      tenv : (ID.t, Type.t) Hashtbl.t -> 
      closureEnv : CollectPartialApps.closure_env ->
      specializer : (value -> Signature.t -> fn) ->
      output_arity : (value -> int) ->  
      fn:fn -> 
      fn 