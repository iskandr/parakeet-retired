open AST 

val mk_node : ?info:ast_info -> ?src:SourceInfo.t -> exp -> node 

val mk_str_node : ?info:ast_info -> ?src:SourceInfo.t -> string -> node 
val update_str_node : node -> string -> node 

val mk_block_node : ?info:ast_info -> ?src:SourceInfo.t -> node list -> node
val update_block_node : node -> node list -> node 

val mk_arr_node : ?info:ast_info -> ?src:SourceInfo.t -> node 
val update_arr_node : node -> node list -> node 

val mk_app_node : ?info:ast_info -> ?src:SourceInfo.t -> node 
val update_app_node : node -> node -> node list -> node 

val mk_prim_node : ?info:ast_info -> ?src:SourceInfo.t -> Prim.prim -> node 
val update_prim_node : node -> Prim.prim -> node  

val mk_var_node : ?info:ast_info -> ?src:SourceInfo.t -> string -> node 
val update_var_node : node -> string -> node  

val mk_def_node : ?info:ast_info -> ?src:SourceInfo.t -> string -> node -> node 
val update_def_node : node -> string -> node -> node 

val mk_lam_node : 
    ?info:ast_info -> ?src:SourceInfo.t -> string list -> node -> node  

val update_lam_node : node -> string list -> node -> node  
    
let mk_if_node ?(astInfo=mk_ast_info()) ?(src=SourceInfo.empty) 
               condNode tNode fNode = 
  mk_node ~astInfo  ~src (If(condNode, tNode, fNode))

let update_if_node ast condNode tNode fNode = 
  {ast with data=If(condNode,tNode,fNode)}

let mk_void_node ?(astInfo=mk_ast_info()) ?(src=SourceInfo.empty) () = 
  mk_node ~astInfo ~src Void 
  
let mk_iftrue_node ?(astInfo=mk_ast_info()) ?(src=SourceInfo.empty) 
   condNode trueNodes =
  let voidNode = mk_void_node() in 
  let blockNode = mk_block_node trueNodes in 
  mk_node ~astInfo ~src (If(condNode, blockNode, voidNode)) 

let mk_int_node ?(astInfo=mk_ast_info()) ?(src=SourceInfo.empty) x = 
  mk_node ~astInfo ~src $ Num (Int32 (Int32.of_int x))  
val update_int_node : node -> int -> node 

let mk_void_node : ?info:ast_info -> ?src:SourceInfo.t -> unit -> node 

(* Array indexing -- special case of App *) 
let mk_idx_node : ?info:ast_info -> ?src:SourceInfo.t -> node -> node -> node 
let mk_eq_node ?info:ast_info -> ?src->SourcInfo.t -> node -> node -> node 
let mk_concat_node : ?info:ast_info -> ?src:SourceInfo.t -> node -> node -> node 


val get_lam_args : node -> string list 
val get_lam_body : node -> node 

(* get ride of nested blocks *)
val flatten_block  : node -> node   
val is_void_recursive : node -> bool 