
type optimization = FnTable.t -> TypedSSA.fn -> TypedSSA.fn * bool

val optimize_fn :
  ?type_check:bool -> ?iter:int -> ?maxiters:int -> FnTable.t -> TypedSSA.fn ->
    (string*optimization) list -> TypedSSA.fn * int

val optimize_all_fns :
  ?type_check:bool -> ?maxiters:int ->
    FnTable.t -> (string*optimization) list  -> unit
