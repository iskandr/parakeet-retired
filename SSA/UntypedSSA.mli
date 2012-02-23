module CoreLanguage : sig
	type value =
	  | Var of ID.t | Num of ParNum.t | Prim of Prim.t | GlobalFn of FnId.t
	type value_node = { value : value; value_src : SrcInfo.t option }
	type value_nodes = value_node list

	type untyped_adverb_info =
	  (value_node, value_nodes, value_nodes option) Adverb.info

	type exp =
	  | Values of value_nodes
	  | Arr of value_nodes
	  | App of value_node * value_nodes
	  | Adverb of untyped_adverb_info * value_nodes
	type exp_node = { exp : exp; exp_src : SrcInfo.t option }

  type phi_node = value_node PhiNode.t
  type phi_nodes = phi_node list

  type stmt =
    | Set of ID.t list * exp_node
    | SetIdx of value_node * value_node list * exp_node
    | If of value_node * block * block * phi_nodes
    | WhileLoop of block * value_node * block *  phi_nodes
  and stmt_node = {
    stmt: stmt;
    stmt_src: SrcInfo.t option;
    stmt_id : StmtId.t;
  }
  and block = stmt_node Block.t

  type fn =
    {
      body : block;
      input_ids : ID.t list;
      output_ids : ID.t list;
      fn_id : FnId.t
    }
end
include module type of CoreLanguage

module PrettyPrinters : sig
  val value_to_str : value -> string
  val value_node_to_str : value_node -> string
  val value_nodes_to_str : value_nodes -> string
  val untyped_adverb_info_to_str : untyped_adverb_info -> string
  val exp_to_str : exp -> string

  val exp_node_to_str : exp_node -> string

  val ids_to_str : ID.t list -> string
  val phi_node_to_str : phi_node -> string
  val phi_nodes_to_str : phi_nodes -> string
  val stmt_to_str : stmt -> string
  val stmt_node_to_str : stmt_node -> string
  val block_to_str :  block -> string

  val fn_to_str : fn -> string
end
include module type of PrettyPrinters

module FnHelpers : sig
	val mk_fn :
	  ?name: string -> input_ids: (ID.t list) -> output_ids: (ID.t list) ->
	    body: block -> fn

	val input_arity : fn -> int
	val output_arity : fn -> int

	val fn_id : fn -> FnId.t
	val find_fn_src_info : fn -> SrcInfo.t option
end
include module type of FnHelpers

val wrap_value : ?src: SrcInfo.t -> value -> value_node
val wrap_exp : ?src : SrcInfo.t -> value_node -> exp_node

val is_empty_exp : exp -> bool
val is_empty_exp_node : exp_node -> bool

