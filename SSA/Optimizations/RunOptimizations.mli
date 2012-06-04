
type optimization = TypedSSA.fn -> TypedSSA.fn * bool

val optimize_fn :
  ?type_check:bool -> ?iter:int -> ?maxiters:int -> TypedSSA.fn ->
    (string*optimization) list -> TypedSSA.fn * int

val optimize_all_fns :
  ?type_check:bool -> ?maxiters:int ->
    ?optimizations:(string*optimization) list  -> unit -> unit
