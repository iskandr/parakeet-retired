
val do_inline : 
  SSA.fundef -> SSA.value_node list -> 
      SSA.block * SSA.exp_node * (ID.t*Type.t) list    
val run_fundef_inliner : FnTable.t -> SSA.fundef -> SSA.fundef * bool  