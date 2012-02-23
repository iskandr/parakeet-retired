
(* Phi-Nodes and statements are shared by both Typed and Untyped *)
(* SSA intermediate languages *)

module StmtId : sig
  type t
  val gen : unit -> t
  val gen_named : string -> t
end

module type CORE_LANGUAGE = sig
  type value
  val value_to_str : value -> string
  type value_node
  val value_node_to_str : value_node -> string


  type exp
  val exp_to_str : exp -> string
  type exp_node
  val exp_node_to_str : exp_node -> string
  val is_empty_exp_node : exp_node -> bool
end

module Make(L : CORE_LANGUAGE) : sig
  type phi_node = {
    phi_id : ID.t;
    phi_left:  L.value_node;
    phi_right: L.value_node;
    phi_src : SrcInfo.t option;
  }
	type phi_nodes = phi_node list

	val phi_node_to_str : phi_node -> string
  val phi_nodes_to_str : phi_nodes -> string

	val phi : ?src:SrcInfo.t -> ID.t -> L.value_node -> L.value_node -> phi_node

	val collect_phi_values : bool -> phi_nodes -> ID.t list * L.value_node list

	type stmt =
	  | Set of ID.t list * L.exp_node
	  | SetIdx of L.value_node * L.value_node list * L.exp_node
	  | If of L.value_node * block * block * phi_nodes
	  (* testBlock, testVal, body, loop header, loop exit *)
	  | WhileLoop of block * L.value_node * block * phi_nodes
	and stmt_node = {
	  stmt: stmt;
	  stmt_src: SrcInfo.t option;
	  stmt_id : StmtId.t;
	}
	and block = stmt_node Block.t

	val wrap_stmt : ?src:SrcInfo.t -> stmt -> stmt_node

	val ids_to_str : ID.t list -> string
	val stmt_to_str : stmt -> string
	val stmt_node_to_str : stmt_node -> string
	val block_to_str :  block -> string
end



