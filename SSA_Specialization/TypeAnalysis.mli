open SSA
 
val type_analysis : 
      (value -> Signature.t -> DynType.t list) -> 
      (CollectPartialApps.closure_env) -> 
      fundef -> 
      Signature.t -> 
      (ID.t, DynType.t) Hashtbl.t       
  