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



  type typed_block = (exp_node, value_node) SSA.block

  val typed_block_to_str : typed_block -> string
  type tenv = Type.t ID.Map.t
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

  val typed_ids_to_str : tenv -> ID.t list -> string

  val fn_to_str : fn -> string
  val find_fn_src_info : fn -> SrcInfo.t option
  val input_arity : fn -> int
  val output_arity : fn -> int
  val input_types : fn -> Type.t list
  val output_types : fn -> Type.t list