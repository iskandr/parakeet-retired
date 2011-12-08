type stmts_and_info = { 
    stmts : Imp.stmt list; 
    new_ids : ID.t list; 
    new_types : ImpType.t list;
    new_shapes : SymbolicShape.shape list;
}


(* rename the variables in the body, and splice them into a loop *) 
val translate_map : ?axes:int list -> fn:SSA.fn -> args:SSA.value list ->  stmts_and_info
val translate : SSA.fn -> ImpType.t list -> Imp.fn 