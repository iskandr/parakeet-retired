open AST

val mk_formal_args : 
  string list -> string list -> node list -> node Args.formal_args

val mk_actual_args : 
  node list -> string list -> node list -> node Args.actual_args


val mk_node : ?info:ast_info -> ?src:SrcInfo.t -> exp -> node

val mk_str_node : ?info:ast_info -> ?src:SrcInfo.t -> string -> node
val update_str_node : node -> string -> node

val mk_block_node : ?info:ast_info -> ?src:SrcInfo.t -> node list -> node
val update_block_node : node -> node list -> node

val mk_array_node : ?info:ast_info -> ?src:SrcInfo.t -> node list -> node
val update_array_node : node -> node list -> node



val mk_call_node : 
  ?info:ast_info -> ?src:SrcInfo.t -> node -> node Args.actual_args -> node

val update_call_node : node -> node -> node Args.actual_args -> node

val mk_prim_node : ?info:ast_info -> ?src:SrcInfo.t -> Prim.t -> node
val update_prim_node : node -> Prim.t -> node

val mk_var_node : ?info:ast_info -> ?src:SrcInfo.t -> string -> node
val update_var_node : node -> string -> node

val mk_assign_node :
      ?info:ast_info -> ?src:SrcInfo.t -> node list -> node -> node
val update_assign_node :
      node -> node list -> node -> node

val mk_lambda_node :
    ?info:ast_info -> ?src:SrcInfo.t -> node Args.formal_args -> node -> node

val update_lambda_node : node -> node Args.formal_args -> node -> node

val mk_if_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node -> node

val update_if_node : node -> node -> node -> node -> node

val mk_none_node : ?info:ast_info -> ?src:SrcInfo.t -> unit -> node

val mk_iftrue_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node list -> node

val mk_int_node : ?info:ast_info -> ?src:SrcInfo.t -> int -> node

val update_int_node : node -> int -> node


(* Array indexing -- special case of App *)
val mk_idx_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> int -> node
val mk_eq_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node
(*val mk_concat_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node*)


val get_lambda_args : node -> node Args.formal_args 
val get_lambda_body : node -> node

(* get ride of nested blocks *)
val flatten_block  : node -> node
val is_void_recursive : node -> bool
