val mk_untyped_prim_fn : Prim.t -> int -> TypedSSA.fn


val mk_typed_scalar_prim :
  Prim.scalar_op -> ?optOutType:Type.t -> Type.t list ->  TypedSSA.fn

val is_scalar_stmt : TypedSSA.stmt_node -> ThreeValuedLogic.t
val is_scalar_block : TypedSSA.block -> ThreeValuedLogic.t


val scalarize_fn : TypedSSA.fn -> Signature.t -> TypedSSA.fn

val specialize_value : TypedSSA.value -> Signature.t -> TypedSSA.fn
val specialize_fn : TypedSSA.fn -> Signature.t -> TypedSSA.fn
val specialize_fn_id : FnId.t -> Signature.t -> TypedSSA.fn