type tenv =  DynType.t ID.Map.t 

val rewrite_block : tenv -> SSA.block -> SSA.block * tenv 
