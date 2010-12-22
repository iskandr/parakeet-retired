open SSA

val rewrite_typed 
    : DynType.t ID.Map.t -> CollectPartialApps.closure_env ->
      (value -> Signature.t -> fundef) -> fundef -> fundef 