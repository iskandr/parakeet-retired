open Base

val add_untyped :  ?optimize:bool -> string -> SSA.Untyped.fn -> unit
val add_typed : ?optimize:bool -> SSA.Typed.fn -> unit

val add_untyped_list :  (string * SSA.Untyped.fn) list -> unit
val add_untyped_map :  SSA.Untyped.fn String.Map.t -> unit

val add_specialization :
  ?optimize:bool -> SSA.Untyped.value -> Signature.t -> SSA.Typed.fn -> unit

val maybe_get_specialization : SSA.Untyped.value -> Signature.t -> FnId.t option

val is_untyped_function : FnId.t -> bool
val get_untyped_function : FnId.t -> SSA.Untyped.fn
val get_typed_function : FnId.t -> SSA.Typed.fn

val get_untyped_name : FnId.t -> string
val get_untyped_id : string -> FnId.t

val get_typed_function_table : unit -> FnTable.t

val have_untyped_function : string -> bool
val get_untyped_arity : FnId.t -> int

val optimize_typed_functions : unit -> unit
val optimize_untyped_functions : unit -> unit

val output_arity_of_typed_fn : FnId.t -> int
val output_arity_of_untyped_fn : FnId.t -> int


