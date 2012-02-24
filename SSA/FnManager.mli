open Base

val add_untyped :  string -> UntypedSSA.fn -> unit
val add_typed : ?optimize:bool -> TypedSSA.fn -> unit

val add_untyped_list :  (string * UntypedSSA.fn) list -> unit
val add_untyped_map :  UntypedSSA.fn String.Map.t -> unit

val add_specialization :
  ?optimize:bool -> UntypedSSA.value -> Signature.t -> TypedSSA.fn -> unit

val maybe_get_specialization : UntypedSSA.value -> Signature.t -> FnId.t option

val is_untyped_function : FnId.t -> bool
val get_untyped_function : FnId.t -> UntypedSSA.fn
val get_typed_function : FnId.t -> TypedSSA.fn

val get_untyped_name : FnId.t -> string
val get_untyped_id : string -> FnId.t

val get_typed_function_table : unit -> FnTable.t

val have_untyped_function : string -> bool

val get_untyped_arity : FnId.t -> int
val output_arity_of_typed_fn : FnId.t -> int
val output_arity_of_untyped_fn : FnId.t -> int


val optimize_typed_functions : unit -> unit
