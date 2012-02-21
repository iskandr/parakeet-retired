
val do_inline :
  TypedSSA.fn -> TypedSSA.value_nodes ->
      TypedSSA.block * TypedSSA.exp_node * (ID.t*Type.t) list
val run_fn_inliner : FnTable.t -> TypedSSA.fn -> TypedSSA.fn * bool