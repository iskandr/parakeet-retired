type def = SingleDef of SSA.exp | CombineDef of ID.Set.t | FunArg

val find_defs : SSA.block -> def ID.Map.t 