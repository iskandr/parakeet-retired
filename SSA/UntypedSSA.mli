open Base 
module CoreLanguage : sig
  type value =
    | Var of ID.t
    | Num of ParNum.t
    | Prim of Prim.t
    | GlobalFn of FnId.t
    | NoneVal

  type value_node = { value : value; value_src : SrcInfo.t option }
  type value_nodes = value_node list

  type adverb_info = (value_node, value_nodes, value_nodes option) Adverb.t

  type exp =
    | Values of value_nodes
    | Tuple of value_nodes
    | Array of value_nodes
    | Call of value_node * (value_node Args.actual_args)
    | Adverb of adverb_info
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
      inputs : value_node Args.formal_args; 
      input_names_to_ids : ID.t String.Map.t; 
      output_ids : ID.t list;
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
  val fn_id_to_str : fn -> string
  val fn_to_str : fn -> string
end
include module type of PrettyPrinters

val wrap_value : ?src: SrcInfo.t -> value -> value_node
val wrap_exp : ?src : SrcInfo.t -> value_node -> exp_node

val is_empty_exp : exp -> bool
val is_empty_exp_node : exp_node -> bool

module ValueHelpers : sig
  val op :  ?src:SrcInfo.t ->  Prim.t -> value_node
  val globalfn : ?src:SrcInfo.t -> FnId.t -> value_node
  val get_id : value_node -> ID.t
  val var : ?src:SrcInfo.t -> ID.t -> value_node
  val num : ?src:SrcInfo.t -> ParNum.t -> value_node
  val bool : ?src:SrcInfo.t -> bool -> value_node
  val int32  : ?src:SrcInfo.t -> int -> value_node
  val float32 : ?src:SrcInfo.t -> float -> value_node
  val float64 : ?src:SrcInfo.t -> float -> value_node
  val is_const : value_node -> bool
  val is_const_int : value_node -> bool
  val get_const : value_node -> ParNum.t
  val get_const_int : value_node -> int

  val lt : value_node
  val lte : value_node
  val gt : value_node
  val gte : value_node
  val eq : value_node
  val neq : value_node
  val plus : value_node
  val zero : value_node
  val one: value_node

end
include module type of ValueHelpers

module ExpHelpers : sig
  val call : value_node -> value_node Args.actual_args -> exp_node
end
include module type of ExpHelpers

module StmtHelpers : sig
  val stmt : ?src:SrcInfo.t -> ?id:StmtId.t -> stmt -> stmt_node
  val set : ?src:SrcInfo.t -> ID.t list -> exp_node -> stmt_node
  val setidx :
    ?src:SrcInfo.t -> value_node -> value_nodes -> exp_node -> stmt_node

end
include module type of StmtHelpers

module FnHelpers : sig
  val mk_fn :
    ?name: string -> 
      inputs : value_node Args.formal_args -> 
        input_names_to_ids : ID.t String.Map.t -> 
          output_ids: (ID.t list) ->
            body: block -> 
              fn
  val input_arity : fn -> int
  val output_arity : fn -> int
  val fn_id : fn -> FnId.t
end
include module type of FnHelpers

module ScalarHelpers : sig
  val is_scalar_exp : exp -> bool
  val is_scalar_exp_node : exp_node -> bool
  val is_scalar_stmt : ?control_flow:bool -> stmt -> bool
  val is_scalar_stmt_node : ?control_flow:bool -> stmt_node -> bool
  val is_scalar_block : ?control_flow:bool -> block -> bool
  val is_scalar_fn : ?control_flow:bool -> fn -> bool
end
include module type of ScalarHelpers
