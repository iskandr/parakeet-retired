
type optimization = FnTable.t -> SSA.Typed.fn -> SSA.Typed.fn * bool

val optimize_fn :
  ?type_check:bool -> ?iter:int -> ?maxiters:int -> FnTable.t -> SSA.Typed.fn ->
    (string*optimization) list -> SSA.Typed.fn * int

val optimize_all_fns :
  ?type_check:bool -> ?maxiters:int ->
    FnTable.t -> (string*optimization) list  -> unit
