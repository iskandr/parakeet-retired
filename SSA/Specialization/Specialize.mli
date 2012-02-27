val mk_untyped_prim_fn : Prim.t -> int -> UntypedSSA.fn


val mk_typed_scalar_prim :
  Prim.scalar_op -> ?optOutType:Type.t -> Type.t list ->  TypedSSA.fn


val scalarize_fn : UntypedSSA.fn -> Signature.t -> TypedSSA.fn

val specialize_value : UntypedSSA.value -> Signature.t -> TypedSSA.fn
val specialize_fn : UntypedSSA.fn -> Signature.t -> TypedSSA.fn
val specialize_fn_id : FnId.t -> Signature.t -> TypedSSA.fn