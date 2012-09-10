open TypedSSA

val const_axes : TypedSSA.value_nodes -> int list

val adverb_with_const_axes :
  ('a, 'b, TypedSSA.value_nodes) Adverb.t -> ('a, 'b, int list) Adverb.t

val max_num_axes_from_array_types : Type.t list -> int
val infer_adverb_axes_from_rank : int -> value_nodes
val infer_adverb_axes_from_types : Type.t list -> value_nodes
val infer_adverb_axes_from_args :
  ?axes:value_nodes -> value_nodes -> value_nodes


val mk_adverb_exp_node :
  ?src:SrcInfo.t -> (FnId.t, value_nodes, value_nodes) Adverb.t -> exp_node

val mk_adverb_fn :
    ?src:SrcInfo.t -> (FnId.t, Type.t list, value_nodes) Adverb.t -> fn

val stmt_has_adverb : TypedSSA.stmt_node -> bool
val block_has_adverb : TypedSSA.block -> bool
val fn_has_adverb : TypedSSA.fn -> bool
