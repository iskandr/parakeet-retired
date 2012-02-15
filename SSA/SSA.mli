(********** VALUES **********)
type value =
  | Var of ID.t
  | Num of ParNum.t
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.t
  | GlobalFn of FnId.t

and value_node = {
  value_type : Type.t;
  value_src : SrcInfo.t option;
  value : value
}
and value_nodes = value_node list


type closure = {
  closure_fn: FnId.t;
  closure_args: value_node list;
  closure_arg_types: Type.t list;
}


type adverb_args = {
  axes : value_nodes option;
  init : value_nodes option;
  args : value_nodes
}

(********** EXPRESSIONS **********)
type exp =
  (* application of arbitrary values used only in untyped code *)
  | App of  value_node * value_nodes
  (* construction of arrays and values used by both typed and untyped ssa *)
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *)
  | Cast of Type.t * value_node
  | Call of FnId.t * value_nodes
  | PrimApp of Prim.t * value_nodes
  | Adverb of Prim.adverb * closure * adverb_args

and exp_node = {
  exp: exp;
  exp_src : SrcInfo.t option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)
  exp_types : Type.t list;
}

(********** STATEMENTS **********)
type stmt =
  | Set of ID.t list * exp_node
  | SetIdx of value_node * value_nodes * value_node
  | If of value_node * block * block * phi_nodes
  (* testBlock, testVal, body, loop header  *)
  | WhileLoop of block * value_node * block * phi_nodes
and stmt_node = {
    stmt: stmt;
    stmt_src: SrcInfo.t option;
    stmt_id : StmtId.t;
}
and block = stmt_node Block.t
and phi_node = {
  phi_id : ID.t;
  phi_left:  value_node;
  phi_right: value_node;
  phi_type : Type.t;
  phi_src : SrcInfo.t option;
}
and phi_nodes = phi_node list


type fn = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list;
  fn_input_types : Type.t list;
  fn_output_types : Type.t list;
  fn_id : FnId.t;
}
and tenv = Type.t ID.Map.t

val is_simple_exp : exp -> bool

val id_list_to_str : ID.t list -> string

val typed_id_to_str : tenv -> ID.t -> string
val typed_id_list_to_str : tenv -> ID.t list -> string


val block_to_str : ?space:string -> ?tenv:tenv ->  block -> string
val stmt_node_to_str : ?space:string -> ?tenv:tenv -> stmt_node -> string
val exp_to_str : exp_node -> string
val value_node_to_str : value_node -> string
val value_nodes_to_str : value_node list -> string

val value_to_str : value -> string
val value_node_list_to_str : ?sep:string -> value_node list -> string
val value_list_to_str : ?sep:string -> value list -> string

val fn_to_str : fn -> string
val closure_to_str : closure -> string
