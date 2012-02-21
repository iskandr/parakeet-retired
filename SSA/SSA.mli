
(* Phi-Nodes and statements are shared by both Typed and Untyped *)
(* SSA intermediate languages *)


type 'a phi_node = {
  phi_id : ID.t;
  phi_left:  'a;
  phi_right: 'a;
  phi_src : SrcInfo.t option;
}
type 'a phi_nodes = 'a phi_node list

val phi_node_to_str :('a -> string) -> 'a phi_node -> string

val phi_nodes_to_str : ('a -> string) -> 'a phi_nodes -> string


val phi : ?src:SrcInfo.t -> ID.t -> 'a -> 'a -> 'a phi_node

val collect_phi_values : bool -> 'a phi_nodes -> ID.t list * 'a list

(* 'a = type of expressions *)
(* 'b = type of values *)
type ('a, 'b) stmt =
  | Set of ID.t list * 'a
  | SetIdx of 'b * 'b list * 'a
  | If of 'b * ('a, 'b) block * ('a, 'b) block * 'b phi_nodes
  (* testBlock, testVal, body, loop header, loop exit *)
  | WhileLoop of ('a, 'b) block * 'b * ('a, 'b) block * 'b phi_nodes
and ('a, 'b) stmt_node = {
  stmt: ('a, 'b) stmt;
  stmt_src: SrcInfo.t option;
  stmt_id : StmtId.t;
}
and ('a,'b) block = ('a,'b) stmt_node Block.t

val wrap_stmt : ?src:SrcInfo.t -> ('a, 'b) stmt -> ('a, 'b) stmt_node

val ids_to_str : ID.t list -> string
val stmt_to_str : ('a -> string) -> ('b -> string) -> ('a, 'b) stmt -> string
val stmt_node_to_str :
  ('a -> string) -> ('b -> string) -> ('a, 'b) stmt_node -> string
val block_to_str : ('a -> string) -> ('b -> string) -> ('a, 'b) block -> string






