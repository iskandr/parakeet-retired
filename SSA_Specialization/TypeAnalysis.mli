open SSA
 
val type_analysis : 
      specializer:(value -> Signature.t -> fundef) ->
      output_arity:(value->int)->  
      closureEnv:CollectPartialApps.closure_env -> 
      fundef:fundef -> 
      signature:Signature.t -> 
      (ID.t, DynType.t) Hashtbl.t       
  
  