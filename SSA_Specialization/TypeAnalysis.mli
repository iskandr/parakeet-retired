open SSA
 
val type_analysis : 
      (value -> Signature.t -> DynType.t list) -> 
      (CollectPartialApps.closure_env) -> 
      fundef -> 
      Signature.t -> 
      DynType.t ID.Map.t      
  