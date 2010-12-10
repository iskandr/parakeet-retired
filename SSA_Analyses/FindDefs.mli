
type def = SingleDef of SSA.exp * int * int | CombineDef of ID.Set.t | FunArg

val find_block_defs : SSA.block -> def ID.Map.t 

val find_function_defs : SSA.fundef -> def ID.Map.t
 