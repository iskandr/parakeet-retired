

type specializer = SSA.value -> Signature.t -> SSA.fundef

val type_eval 
  : InterpState.t -> specializer -> CollectPartialApps.closure_env -> 
      SSA.fundef -> DynType.t ID.Map.t    
   