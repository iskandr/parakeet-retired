
val do_inline : 
  SSA.fn -> SSA.value_node list -> 
      SSA.block * SSA.exp_node * (ID.t*Type.t) list    
val run_fn_inliner : FnTable.t -> SSA.fn -> SSA.fn * bool  