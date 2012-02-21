type value = | Var of ID.t | Num of ParNum.t

val value_to_str : value -> string

type value_node =
  { value : value; value_type : Type.t; value_src : SrcInfo.t option }

val value_node_to_str : value_node -> string

type value_nodes = value_node list

val value_nodes_to_str : value_nodes -> string

val wrap_value : ?src: SrcInfo.t -> value -> Type.t -> value_node

type typed_adverb_info = (FnId.t, value_nodes, value_nodes) Adverb.info

val typed_adverb_info_to_str : typed_adverb_info -> string

type exp =
  | Values of value_nodes
  | Arr of value_nodes
  | Call of FnId.t * value_nodes
  | PrimApp of Prim.t * value_nodes
  | Adverb of typed_adverb_info * value_nodes

val exp_to_str : exp -> string

type exp_node =
  { exp : exp; exp_src : SrcInfo.t option; exp_types : Type.t list }

val wrap_exp : value_node -> exp_node

type typed_block = (exp_node, value_node) SSA.block

val typed_block_to_str : typed_block -> string

type tenv = Type.t ID.Map.t

type fn =
  { body : typed_block; tenv : tenv; input_ids : ID.t list;
    output_ids : ID.t list; fn_input_types : Type.t list;
    fn_output_types : Type.t list; fn_id : FnId.t
  }

val mk_fn :
  ?name: string -> tenv: tenv -> input_ids: (ID.t list) ->
    output_ids: (ID.t list) -> body: typed_block -> fn

val typed_id_to_str : tenv -> ID.t -> string

val typed_ids_to_str : tenv -> ID.t list -> string

val fn_to_str : fn -> string

val find_fn_src_info : fn -> SrcInfo.t option

val input_arity : fn -> int

val output_arity : fn -> int

val input_types : fn -> Type.t list

val output_types : fn -> Type.t list

val fn_builder :
  ?name: string -> input_types: (Type.t list) -> output_types: (Type.t list) ->
    ?local_types: (Type.t list) ->
      ((value_nodes * value_nodes * value_nodes) -> stmt_node list) -> fn

(***
    helpers for values
 ***)

val var : ?src:SrcInfo.t -> ?ty:Type.t -> ID.t -> value_node
val op :  ?src:SrcInfo.t -> ?ty:Type.t -> Prim.t -> value_node

val globalfn : ?src:SrcInfo.t -> ?ty:Type.t -> FnId.t -> value_node

val num : ?src:SrcInfo.t -> ?ty:Type.t -> ParNum.t -> value_node

val bool : ?src:SrcInfo.t -> bool -> value_node
val int32  : ?src:SrcInfo.t -> int -> value_node

val float32 : ?src:SrcInfo.t -> float -> value_node
val float64 : ?src:SrcInfo.t -> float -> value_node

val is_const : value_node -> bool
val is_const_int : value_node -> bool
val get_const : value_node -> ParNum.t
val get_const_int : value_node -> int

