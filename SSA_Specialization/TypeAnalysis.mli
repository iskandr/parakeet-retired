open SSA
 
val type_analysis : 
      specializer:InterpState.t -> value -> Signature.t -> fundef ->
      interpState:InterpState.t ->  
      closureEnv:CollectPartialApps.closure_env -> 
      fundef:fundef -> 
      signature:Signature.t -> 
      (ID.t, DynType.t) Hashtbl.t       
  