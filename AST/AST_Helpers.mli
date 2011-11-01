open AST 

val mk_node : ?info:ast_info -> ?src:SrcInfo.t -> exp -> node 

val mk_str_node : ?info:ast_info -> ?src:SrcInfo.t -> string -> node 
val update_str_node : node -> string -> node 

val mk_block_node : ?info:ast_info -> ?src:SrcInfo.t -> node list -> node
val update_block_node : node -> node list -> node 

val mk_arr_node : ?info:ast_info -> ?src:SrcInfo.t -> node 
val update_arr_node : node -> node list -> node 

val mk_app_node : ?info:ast_info -> ?src:SrcInfo.t -> node 
val update_app_node : node -> node -> node list -> node 

val mk_prim_node : ?info:ast_info -> ?src:SrcInfo.t -> Prim.prim -> node 
val update_prim_node : node -> Prim.prim -> node  

val mk_var_node : ?info:ast_info -> ?src:SrcInfo.t -> string -> node 
val update_var_node : node -> string -> node  

val mk_def_node : ?info:ast_info -> ?src:SrcInfo.t -> string -> node -> node 
val update_def_node : node -> string -> node -> node 

val mk_lam_node : 
    ?info:ast_info -> ?src:SrcInfo.t -> string list -> node -> node  

val update_lam_node : node -> string list -> node -> node  
    
val mk_if_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node -> node 

val update_if_node : node -> node -> node -> node -> node 

val mk_void_node : ?info:ast_info -> ?src:SrcInfo.t -> node 
  
val mk_iftrue_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node list -> node 

val mk_int_node : ?info:ast_info -> ?src:SrcInfo.t -> int -> node 

val update_int_node : node -> int -> node 


(* Array indexing -- special case of App *) 
val mk_idx_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node 
val mk_eq_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node 
val mk_concat_node : ?info:ast_info -> ?src:SrcInfo.t -> node -> node -> node 


val get_lam_args : node -> string list 
val get_lam_body : node -> node 

(* get ride of nested blocks *)
val flatten_block  : node -> node   
val is_void_recursive : node -> bool 