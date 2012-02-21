
val do_inline :
  SSA.Typed.fn -> SSA.Typed.value_nodes ->
      SSA.Typed.block * SSA.Typed.exp_node * (ID.t*Type.t) list
val run_fn_inliner : FnTable.t -> SSA.Typed.fn -> SSA.Typed.fn * bool