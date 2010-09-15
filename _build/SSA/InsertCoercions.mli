type tenv = (ID.t, DynType.t) PMap.t 

val rewrite_block : tenv -> SSA.block -> SSA.block * tenv 

val rewrite_stmt : tenv -> SSA.stmt_node -> SSA.block * tenv

val rewrite_exp : tenv -> SSA.exp_node -> SSA.exp_node * SSA.block * tenv

val rewrite_value : tenv -> SSA.value_node -> SSA.value_node * SSA.block * tenv 
    
                