val optimize_fn : FnTable.t -> TypedSSA.fn -> TypedSSA.fn * bool 
val init_get_typed_fn : (FnId.t -> TypedSSA.fn) -> unit
val init_add_typed_fn : (?optimize:bool -> TypedSSA.fn -> unit)-> unit
