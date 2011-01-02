open SSA

val rewrite_typed 
    : (ID.t, DynType.t) Hashtbl.t -> CollectPartialApps.closure_env ->
      (value -> Signature.t -> fundef) -> fundef -> fundef 