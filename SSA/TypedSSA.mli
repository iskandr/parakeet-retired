module CoreLanguage : sig
  type value = | Var of ID.t | Num of ParNum.t
	type value_node =
	  { value : value; value_type : Type.t; value_src : SrcInfo.t option }

	type value_nodes = value_node list
	type adverb_info = (FnId.t, value_nodes, value_nodes) Adverb.info

	type exp =
	  | Values of value_nodes
	  | Arr of value_nodes
	  | Call of FnId.t * value_nodes
	  | PrimApp of Prim.t * value_nodes
	  | Adverb of adverb_info * value_nodes
    | Cast of Type.t * value_node

	type exp_node =
	  { exp : exp; exp_src : SrcInfo.t option; exp_types : Type.t list }

  type phi_node = value_node PhiNode.t
  type phi_nodes = phi_node list

  type stmt =
    | Set of ID.t list * exp_node
    | SetIdx of value_node * value_node list * exp_node
    | If of value_node * block * block * phi_nodes
    (* testBlock, testVal, body, loop header, loop exit *)
    | WhileLoop of block * value_node * block * phi_nodes
  and stmt_node = {
    stmt: stmt;
    stmt_src: SrcInfo.t option;
    stmt_id : StmtId.t;
  }
  and block = stmt_node Block.t
  type tenv = Type.t ID.Map.t
  type fn =
    {
      body : block;
      tenv : tenv;
      input_ids : ID.t list;
      output_ids : ID.t list;
      fn_input_types : Type.t list;
      fn_output_types : Type.t list;
      fn_id : FnId.t
    }
end
include module type of CoreLanguage

module PrettyPrinters : sig
  val value_to_str : value -> string
  val value_node_to_str : value_node -> string
  val value_nodes_to_str : value_nodes -> string
  val adverb_info_to_str : adverb_info -> string
  val exp_to_str : exp -> string

  val exp_node_to_str : exp_node -> string

  val phi_node_to_str : phi_node -> string
  val phi_nodes_to_str : phi_nodes -> string
  val stmt_to_str : stmt -> string
  val stmt_node_to_str : stmt_node -> string
  val block_to_str :  block -> string

  val typed_id_to_str : tenv -> ID.t -> string
  val typed_ids_to_str : tenv -> ID.t list -> string
  val fn_to_str : fn -> string
end
include module type of PrettyPrinters


val wrap_value : ?src: SrcInfo.t ->  Type.t -> value -> value_node
val wrap_exp : value_node -> exp_node
val wrap_stmt : ?src:SrcInfo.t -> stmt -> stmt_node

val empty_stmt : stmt_node
val is_empty_exp : exp -> bool
val is_empty_exp_node : exp_node -> bool
val is_empty_stmt : stmt_node -> bool

module ValueHelpers : sig
    val get_id : value_node -> ID.t
    val get_ids : value_nodes -> ID.t list
    val var : ?src:SrcInfo.t -> Type.t -> ID.t -> value_node
    val num : ?src:SrcInfo.t -> ParNum.t -> value_node
    val bool : ?src:SrcInfo.t -> bool -> value_node
    val int32  : ?src:SrcInfo.t -> int -> value_node
    val float32 : ?src:SrcInfo.t -> float -> value_node
    val float64 : ?src:SrcInfo.t -> float -> value_node
    val is_const : value_node -> bool
    val is_const_int : value_node -> bool
    val get_const : value_node -> ParNum.t
    val get_const_int : value_node -> int
end
include module type of ValueHelpers

module FnHelpers  : sig
  val mk_fn :
    ?name: string -> tenv: tenv -> input_ids: (ID.t list) ->
      output_ids: (ID.t list) -> body: block -> fn

  val find_fn_src_info : fn -> SrcInfo.t option
  val input_arity : fn -> int
  val output_arity : fn -> int
  val input_types : fn -> Type.t list
  val output_types : fn -> Type.t list

  val fn_builder :
    ?name: string ->
      input_types: (Type.t list) ->
        output_types: (Type.t list) ->
          ?local_types: (Type.t list) ->
            ((value_nodes * value_nodes * value_nodes) -> stmt_node list) -> fn
end
include module type of FnHelpers

module ExpHelpers : sig
  val primapp :
   ?src:SrcInfo.t -> Prim.t -> output_types:Type.t list ->
     value_node list -> exp_node

  val arr : ?src:SrcInfo.t -> Type.t list -> value_node list -> exp_node

  val val_exp : ?src:SrcInfo.t -> Type.t -> value -> exp_node

  val vals_exp : ?src:SrcInfo.t -> Type.t list -> value list -> exp_node

  val cast : ?src:SrcInfo.t -> Type.t -> value_node -> exp_node
  val exp :  ?src:SrcInfo.t -> Type.t list -> exp -> exp_node
  val call :
    ?src:SrcInfo.t -> FnId.t -> Type.t list  -> value_node list -> exp_node
end
include module type of ExpHelpers

module StmtHelpers : sig
  val stmt : ?src:SrcInfo.t -> ?id:StmtId.t -> stmt -> stmt_node
  val set : ?src:SrcInfo.t -> ID.t list -> exp_node -> stmt_node
  val set_vals : ?src:SrcInfo.t -> value_nodes -> exp_node -> stmt_node
  val setidx :
    ?src:SrcInfo.t -> value_node -> value_nodes -> exp_node -> stmt_node
end
include module type of StmtHelpers
