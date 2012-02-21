
type 'a phi_node = {
  phi_id : ID.t;
  phi_left:  'a;
  phi_right: 'a;
  phi_src : SrcInfo.t option;
}
type 'a phi_nodes = 'a phi_node list

val phi_node_to_str :('a -> string) -> 'a phi_node -> string

val phi_nodes_to_str : ('a -> string) -> 'a phi_nodes -> string

type ('a, 'b) stmt =
  | Set of ID.t list * 'a
  | SetIdx of 'a * 'a list * 'a
  | If of 'b * ('a, 'b) block * ('a, 'b) block * 'b phi_nodes
  (* testBlock, testVal, body, loop header, loop exit *)
  | WhileLoop of ('a, 'b) block * 'b * ('a, 'b) block * 'b phi_nodes
and ('a, 'b) stmt_node = {
  stmt: ('a, 'b) stmt;
  stmt_src: SrcInfo.t option;
  stmt_id : StmtId.t;
}
and ('a,'b) block = ('a,'b) stmt_node Block.t

val id_list_to_str : ID.t list -> string
val stmt_to_str : ('a -> string) -> ('b -> string) -> ('a, 'b) stmt -> string
val stmt_node_to_str :
  ('a -> string) -> ('b -> string) -> ('a, 'b) stmt_node -> string
val block_to_str : ('a -> string) -> ('b -> string) -> ('a, 'b) block -> string


module Untyped : sig
  type value =
    | Var of ID.t
    | Num of ParNum.t
    | Prim of Prim.t
    | GlobalFn of FnId.t

  val value_to_str : value -> string

  type value_node = { value : value; value_src : SrcInfo.t option; }
  val value_node_to_str : value_node -> string

  type value_nodes = value_node list
  val value_nodes_to_str : value_nodes -> string

  type untyped_adverb_info =
    (value_node, value_nodes, value_nodes option) Adverb.info

  val untyped_adverb_info_to_str : untyped_adverb_info -> string

  type exp =
    | Values of value_nodes
    | Arr of value_nodes
    | App of value_node * value_nodes
    | Adverb of untyped_adverb_info * value_nodes
  val exp_to_str : exp -> string

  type exp_node = { exp : exp; exp_src : SrcInfo.t option }
  val exp_node_to_str : exp_node -> string

  type untyped_block = (exp_node, value_node) block
  type fn = {
    body: untyped_block;
    input_ids: ID.t list;
    output_ids: ID.t list;
    fn_id : FnId.t;
  }
  val input_arity : fn -> int
  val output_arity : fn -> int
  val fn_id : fn -> FnId.t
  val find_fn_src_info : fn -> SrcInfo.t option
end


module Typed : sig
  type value = Var of ID.t | Num of ParNum.t
  val value_to_str : value -> string

  type value_node = {
    value : value;
    value_type : Type.t;
    value_src : SrcInfo.t option;
  }
  val value_node_to_str : value_node -> string

  type value_nodes = value_node list
  val value_nodes_to_str : value_nodes -> string

  type typed_adverb_info = (FnId.t, value_nodes, value_nodes) Adverb.info

  val typed_adverb_info_to_str : typed_adverb_info -> string

  type exp =
    | Values of value_nodes
    | Arr of value_nodes
    | Call of FnId.t * value_nodes
    | PrimApp of Prim.t * value_nodes
    | Adverb of typed_adverb_info * value_nodes

  val exp_to_str : exp -> string

  type exp_node = {
    exp: exp;
    exp_src : SrcInfo.t option;
    exp_types : Type.t list
  }

  type tenv = Type.t ID.Map.t

  type typed_block = (exp_node, value_node) block

  val typed_block_to_str : typed_block -> string

  type fn = {
    body: typed_block;
    tenv : tenv;
    input_ids: ID.t list;
    output_ids: ID.t list;
    fn_input_types : Type.t list;
    fn_output_types : Type.t list;
    fn_id : FnId.t;
  }

  val typed_id_to_str : tenv -> ID.t -> string

  val typed_id_list_to_str : tenv -> ID.t list -> string

  val fn_to_str : fn -> string
  val find_fn_src_info : fn -> SrcInfo.t option
  val input_arity : fn -> int
  val output_arity : fn -> int
  val input_types : fn -> Type.t list
  val output_types : fn -> Type.t list
end






