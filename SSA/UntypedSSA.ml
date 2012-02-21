  type value =
    | Var of ID.t
    | Num of ParNum.t
    | Prim of Prim.t
    | GlobalFn of FnId.t

  let value_to_str = function
    | Var id -> ID.to_str id
    | Num n -> ParNum.to_str n
    | Prim p -> "prim(" ^ Prim.to_str p ^ ")"
    | GlobalFn fnId -> FnId.to_str fnId

  type value_node = { value : value; value_src : SrcInfo.t option; }
  let value_node_to_str valNode = value_to_str valNode.value

  type value_nodes = value_node list
  let value_nodes_to_str valNodes =
    String.concat ", " (List.map value_node_to_str valNodes)

  let wrap_value ?src value = { value = value; value_src = src }

  type untyped_adverb_info =
    (value_node, value_nodes, value_nodes option) Adverb.info

  let untyped_adverb_info_to_str info =
    let opt_to_str = Option.map_default value_nodes_to_str "none" in
    Adverb.info_to_str value_node_to_str value_nodes_to_str opt_to_str info

  type exp =
    | Values of value_nodes
    | Arr of value_nodes
    | App of value_node * value_nodes
    | Adverb of untyped_adverb_info * value_nodes

  let exp_to_str = function
    | Values vs -> sprintf "values(%s)" (value_nodes_to_str vs)
    | Arr elts -> sprintf "array(%s)" (value_nodes_to_str elts)
    | App (f, args) ->
      sprintf "%s(%s)" (value_node_to_str f) (value_nodes_to_str args)
    | Adverb (info, args) ->
      Printf.sprintf "%s(%s)"
        (untyped_adverb_info_to_str info)
        (value_nodes_to_str args)

  type exp_node = { exp : exp; exp_src : SrcInfo.t option }
  let exp_node_to_str expNode = exp_to_str expNode.exp

  type untyped_block = (exp_node, value_node) SSA.block

  let untyped_block_to_str : untyped_block -> string =
    block_to_str exp_node_to_str value_node_to_str

  type fn = {
    body: untyped_block;
    input_ids: ID.t list;
    output_ids: ID.t list;
    fn_id : FnId.t;
  }

  let fn_to_str (fundef:fn) =
    let name = FnId.to_str fundef.fn_id in
    let inputs = ids_to_str fundef.input_ids in
    let outputs = ids_to_str fundef.output_ids in
    let body = untyped_block_to_str fundef.body in
    wrap_str (sprintf "def %s(%s)=>(%s):\n%s" name inputs outputs body)

  let mk_fn ?name ~input_ids ~output_ids ~body : fn =
    let fnId =
      match name with | Some name -> FnId.gen_named name | None -> FnId.gen()
    in
    {
      body = body;
      input_ids = input_ids;
      output_ids = output_ids;
      fn_id = fnId;
    }

  let find_fn_src_info {body} = get_block_src_info body

  let input_arity {input_ids} = List.length input_ids
  let output_arity {output_ids} = List.length output_ids
  let fn_id {fn_id} = fn_id
