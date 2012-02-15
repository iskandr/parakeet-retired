
val rewrite_typed :
      tenv : (ID.t, Type.t) Hashtbl.t ->
      specializer : (SSA.value -> Signature.t -> SSA.fn) ->
      fn:SSA.fn -> SSA.fn