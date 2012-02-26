

type t
val add : ?opt_queue:bool -> TypedSSA.fn -> t -> unit
val find : FnId.t -> t -> TypedSSA.fn
val find_option : FnId.t -> t -> TypedSSA.fn option
val mem : FnId.t -> t -> bool

val create :  int -> t
val have_unoptimized : t -> bool
val get_unoptimized : t -> TypedSSA.fn
val update : TypedSSA.fn -> t -> unit
val get_arity :  FnId.t -> t -> int