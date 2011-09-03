open SSA

val rewrite_typed : 
      tenv:(ID.t, Type.t) Hashtbl.t -> 
      closureEnv:CollectPartialApps.closure_env ->
      specializer:(value -> Signature.t -> fundef) ->
      output_arity:(value -> int) ->  
      fundef:fundef -> 
        fundef 