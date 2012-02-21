

type t
val add : ?opt_queue:bool -> SSA.Typed.fn -> t -> unit
val find : FnId.t -> t -> SSA.Typed.fn
val find_option : FnId.t -> t -> SSA.Typed.fn option
val mem : FnId.t -> t -> bool

val create :  int -> t
val have_unoptimized : t -> bool
val get_unoptimized : t -> SSA.Typed.fn
val update : SSA.Typed.fn -> t -> unit
val get_arity :  FnId.t -> t -> int