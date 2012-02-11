open SSA


val max_num_axes_from_array_types : Type.t list -> int
val infer_adverb_axes_from_rank : ?axes:value_nodes -> int -> value_nodes
val infer_adverb_axes_from_args :
  ?axes:value_nodes -> value_nodes -> value_nodes

(* These all require us to look up information from typed function arguments *)

val mk_map :
   ?src:SrcInfo.t -> closure -> ?axes:value_nodes  -> value_nodes -> exp_node

val mk_reduce :
    ?src:SrcInfo.t -> closure -> ?axes:value_nodes ->
        value_node list -> value_nodes -> exp_node

val mk_scan :
   ?src:SrcInfo.t -> closure -> ?axes:value_nodes ->
    value_nodes -> value_nodes -> exp_node
