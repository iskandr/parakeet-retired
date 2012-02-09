val mk_untyped_prim_fn : Prim.prim -> int -> SSA.fn
val mk_typed_map_fn : ?src:SrcInfo.t -> SSA.fn -> Type.t list -> SSA.fn
val mk_typed_scalar_prim :
    Prim.scalar_op -> ?optOutType:Type.t -> Type.t list ->  SSA.fn

val is_scalar_stmt : SSA.stmt_node -> ThreeValuedLogic.t
val is_scalar_block : SSA.block -> ThreeValuedLogic.t


val scalarize_fn : SSA.fn -> Signature.t -> SSA.fn

val specialize_value : SSA.value -> Signature.t -> SSA.fn
val specialize_fn : SSA.fn -> Signature.t -> SSA.fn
val specialize_fn_id : FnId.t -> Signature.t -> SSA.fn