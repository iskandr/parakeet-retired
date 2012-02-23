(* rename the variables in the body, and splice them into a loop *)
(*val translate_map : ?axes:int list -> fn:SSA.fn -> args:SSA.value list ->  stmts_and_info*)
val translate_fn : SSA.fn -> ImpType.t list -> Imp.fn
