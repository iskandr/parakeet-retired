open SSA

(* These all require us to look up information from typed function arguments *)

val mk_map :
   ?src:SrcInfo.t -> closure -> ?axes:int list -> value_node list -> exp_node

val mk_reduce :
    ?src:SrcInfo.t -> closure -> ?axes:int list ->
        value_node list -> value_node list -> exp_node

val mk_scan :
   ?src:SrcInfo.t -> closure -> ?axes:int list ->
        value_node list -> value_node list -> exp_node
