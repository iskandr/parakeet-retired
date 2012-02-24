open SSA

val max_num_axes_from_array_types : Type.t list -> int
val infer_adverb_axes_from_rank : int -> value_nodes
val infer_adverb_axes_from_types : Type.t list -> value_nodes
val infer_adverb_axes_from_args :
  ?axes:value_nodes -> value_nodes -> value_nodes

(* These all require us to look up information from typed function arguments *)


val closure_input_types : TypedSSA.closure -> Type.t list
val closure_output_types : TypedSSA.closure -> Type.t list

val mk_adverb :
  ?src:SrcInfo.t -> Prim.adverb -> closure -> ?axes:value_nodes ->
    ?init:value_nodes -> value_nodes -> exp_node

val mk_adverb_fn :
    ?src:SrcInfo.t ->
    adverb:Prim.adverb ->
    nested_fn:TypedSSA.fn ->
    ?axes:TypedSSA.value_nodes ->
    ?init:TypedSSA.value_nodes ->
    ?fixed_types:Type.t list ->
    array_types:Type.t list ->
    TypedSSA.fn

val stmt_has_adverb : TypedSSA.stmt_node -> bool
val block_has_adverb : TypedSSA.block -> bool
val fn_has_adverb : TypedSSA.fn -> bool