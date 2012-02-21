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

  type untyped_block = (exp_node, value_node) SSA.block
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