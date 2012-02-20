open SSA

val max_num_axes_from_array_types : Type.t list -> int
val infer_adverb_axes_from_rank : int -> value_nodes
val infer_adverb_axes_from_types : Type.t list -> value_nodes
val infer_adverb_axes_from_args :
  ?axes:value_nodes -> value_nodes -> value_nodes

(* These all require us to look up information from typed function arguments *)


val closure_input_types : SSA.closure -> Type.t list
val closure_output_types : SSA.closure -> Type.t list

val mk_adverb :
  ?src:SrcInfo.t -> Prim.adverb -> closure -> ?axes:value_nodes ->
    ?init:value_nodes -> value_nodes -> exp_node

val mk_adverb_fn :
    ?src:SrcInfo.t ->
    adverb:Prim.adverb ->
    nested_fn:SSA.fn ->
    ?axes:SSA.value_nodes ->
    ?init:SSA.value_nodes ->
    ?fixed_types:Type.t list ->
    array_types:Type.t list ->
    SSA.fn
