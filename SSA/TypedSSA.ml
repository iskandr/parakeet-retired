  type value = Var of ID.t | Num of ParNum.t
  let value_to_str = function
    | Var id -> ID.to_str id
    | Num n -> ParNum.to_str n

  type value_node = {
    value : value;
    value_type : Type.t;
    value_src : SrcInfo.t option;
  }
  let value_node_to_str valNode =
    sprintf "%s : %s"
      (value_to_str valNode.value)
      (Type.to_str valNode.value_type)

  type value_nodes = value_node list
  let value_nodes_to_str valNodes =
    String.concat ", " (List.map value_node_to_str valNodes)


  type typed_adverb_info = (FnId.t, value_nodes, value_nodes) Adverb.info

  let typed_adverb_info_to_str info =
    Adverb.info_to_str FnId.to_str value_nodes_to_str value_nodes_to_str info

  type exp =
    | Values of value_nodes
    | Arr of value_nodes
    | Call of FnId.t * value_nodes
    | PrimApp of Prim.t * value_nodes
    | Adverb of typed_adverb_info * value_nodes

  let exp_to_str = function
    | Values vs -> sprintf "values(%s)" (value_nodes_to_str vs)
    | Arr elts -> sprintf "array(%s)" (value_nodes_to_str elts)
    | Call (fnId, args) ->
      sprintf "%s(%s)" (FnId.to_str fnId) (value_nodes_to_str args)
    | PrimApp (p, args) ->
      sprintf "prim[%s](%s)" (Prim.to_str p) (value_nodes_to_str args)
    | Adverb (info, args) ->
      Printf.sprintf "%s(%s)"
        (typed_adverb_info_to_str info)
        (value_nodes_to_str args)

  type exp_node = {
    exp: exp;
    exp_src : SrcInfo.t option;
    exp_types : Type.t list
  }
  let exp_node_to_str {exp} = exp_to_str exp

  type typed_block = (exp_node, value_node) block

  let typed_block_to_str : typed_block -> string =
    block_to_str exp_node_to_str value_node_to_str
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

  let typed_id_to_str tenv id =
    (ID.to_str id) ^ " : " ^ (Type.to_str (ID.Map.find id tenv))

  let typed_ids_to_str tenv ids =
    String.concat ", " (List.map (typed_id_to_str tenv) ids)

  let fn_to_str (fundef:fn) =
    let name = FnId.to_str fundef.fn_id in
    let inputs = typed_ids_to_str fundef.tenv fundef.input_ids in
    let outputs = typed_ids_to_str fundef.tenv fundef.output_ids in
    let body = typed_block_to_str fundef.body in
    wrap_str (sprintf "def %s(%s)=>(%s):\n%s" name inputs outputs body)

  let find_fn_src_info {body} = get_block_src_info body

  let input_arity {input_ids} = List.length input_ids
  let output_arity {output_ids} = List.length output_ids
  let input_types {fn_input_types} = fn_input_types
  let output_types {fn_output_types} = fn_output_types
  let fn_id {fn_id} = fn_id
