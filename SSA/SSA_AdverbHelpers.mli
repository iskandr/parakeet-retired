open TypedSSA

val max_num_axes_from_array_types : Type.t list -> int
val infer_adverb_axes_from_rank : int -> value_nodes
val infer_adverb_axes_from_types : Type.t list -> value_nodes
val infer_adverb_axes_from_args :
  ?axes:value_nodes -> value_nodes -> value_nodes


val mk_adverb :
  ?src:SrcInfo.t ->
    info : (FnId.t, value_nodes, value_nodes) Adverb.info ->
      args:value_nodes ->
        exp_node

val mk_adverb_fn :
    ?src:SrcInfo.t ->
      (fn, Type.t list, value_nodes option) Adverb.info ->
        Type.t list ->
          fn

val stmt_has_adverb : TypedSSA.stmt_node -> bool
val block_has_adverb : TypedSSA.block -> bool
val fn_has_adverb : TypedSSA.fn -> bool