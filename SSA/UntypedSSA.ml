open Base
open Printf

module CoreLanguage = struct
  type value =
    | Var of ID.t
    | Num of ParNum.t
    | Prim of Prim.t
    | GlobalFn of FnId.t
    | NoneVal

  type value_node = { value : value; value_src : SrcInfo.t option; }
  type value_nodes = value_node list

  type adverb_info = (value_node, value_nodes, value_nodes option) Adverb.t

  type exp =
    | Values of value_nodes
    | Tuple of value_nodes
    | Array of value_nodes
    | Call of value_node * value_node Args.actual_args
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

  type fn = {
    body: block;
    inputs : value_node Args.formal_args; 
    input_names_to_ids : ID.t String.Map.t; 
    output_ids: ID.t list;
    fn_id : FnId.t;
  }
end
include CoreLanguage

module PrettyPrinters = struct
  let value_to_str = function
    | Var id -> ID.to_str id
    | Num n -> ParNum.to_str n
    | Prim p -> "prim(" ^ Prim.to_str p ^ ")"
    | GlobalFn fnId -> FnId.to_str fnId
    | NoneVal -> "none"

  let value_node_to_str valNode = value_to_str valNode.value

  let value_nodes_to_str valNodes =
    String.concat ", " (List.map value_node_to_str valNodes)

  let adverb_info_to_str info =
    let opt_to_str = Option.map_default value_nodes_to_str "none" in
    Adverb.to_str info value_node_to_str value_nodes_to_str opt_to_str

  let args_to_str args =
    Args.actual_args_to_str ~value_to_str:value_node_to_str args
  
  let exp_to_str = function
    | Values [v] -> value_node_to_str v
    | Values vs -> sprintf "values(%s)" (value_nodes_to_str vs)
    | Tuple vs -> sprintf "tuple(%s)" (value_nodes_to_str vs)
    | Array elts -> sprintf "array(%s)" (value_nodes_to_str elts)
    | Call (f, args) ->
      sprintf "%s(%s)" (value_node_to_str f) (args_to_str args)
    | Adverb info -> adverb_info_to_str info
  let exp_node_to_str expNode = exp_to_str expNode.exp

  let phi_node_to_str = PhiNode.to_str value_node_to_str
  let phi_nodes_to_str phiNodes =
    String.concat "\n" (List.map phi_node_to_str phiNodes)

  let rec stmt_to_str stmt =
    match stmt with
    | Set (ids, rhs) ->
      sprintf "%s = %s" (ID.list_to_str ids) (exp_node_to_str rhs)
    | SetIdx (arr, indices, rhs) ->
      sprintf "%s[%s] = %s"
      (value_node_to_str arr)
      (String.concat ", " (List.map value_node_to_str indices))
      (exp_node_to_str rhs)
    | If (cond, tBlock, fBlock, phiNodes) ->
      sprintf "if %s then%s\nelse%s\nmerge%s"
        (value_node_to_str cond)
        (Base.indent_newlines (block_to_str tBlock))
        (Base.indent_newlines (block_to_str fBlock))
        (Base.indent_newlines $ "\n\n" ^ (phi_nodes_to_str phiNodes))
    | WhileLoop (testBlock, testVal, body, phiNodes) ->
      sprintf "while %s\nloop header%s\ndo%s\nmerge%s"
        (Base.indent_newlines (block_to_str testBlock))
        (value_node_to_str testVal)
        (Base.indent_newlines (block_to_str body))
        (Base.indent_newlines $ "\n" ^ (phi_nodes_to_str phiNodes))

  and stmt_node_to_str {stmt} = stmt_to_str stmt
  and block_to_str block =
    Base.indent_newlines (Block.to_str stmt_node_to_str block)


  let fn_id_to_str (fundef:fn) = FnId.to_str fundef.fn_id

  let fn_to_str (fundef:fn) =
    let name = fn_id_to_str fundef in
    let inputStr =
      Args.formal_args_to_str ~value_to_str:value_node_to_str 
      fundef.inputs 
    in
    let outputs = ID.list_to_str fundef.output_ids in
    let body = block_to_str fundef.body in
    (sprintf "def %s(%s)=>(%s):%s" name inputStr outputs body)

end
include PrettyPrinters

let wrap_value ?src value = { value = value; value_src = src }
let wrap_exp ?src valNode =
    { exp = Values [valNode]; exp_src = src}

let wrap_stmt ?src stmt =
    { stmt = stmt; stmt_src = src; stmt_id = StmtId.gen() }

let is_empty_exp = function Values [] -> true | _ -> false
let is_empty_exp_node {exp} = is_empty_exp exp


module ValueHelpers = struct
    (* get the id of a variable value node *)
  let get_id valNode = match valNode.value with
    | Var id -> id
    | other -> failwith $ Printf.sprintf
       "[SSA->get_id] expected variable, received %s"
       (value_to_str other)

  let var ?src (id:ID.t) : value_node = {value = Var id; value_src = src}
  let op ?src op = wrap_value ?src  (Prim op)

  let globalfn ?src (id:FnId.t) : value_node=
    { value = GlobalFn id; value_src = src}
  let num ?src n = wrap_value ?src (Num n)
  let bool ?src b = num ?src (ParNum.Bool b)
  let int32 ?src i = num ?src (ParNum.coerce_int i Type.Int32T)
  let float32 ?src f = num ?src (ParNum.Float32 f)
  let float64 ?src f = num ?src (ParNum.Float64 f)
  let is_const {value} = match value with | Num _ -> true | _ -> false
  let is_const_int {value} = match value with
    | Num n -> ParNum.is_int n
    | _ -> false
  let get_const {value} = match value with
    | Num n -> n
    | _ -> failwith "Not a constant"
  let get_const_int {value} = match value with
    | Num n -> ParNum.to_int n
    | _ -> failwith "Not an integer"

  let lt = wrap_value (Prim (Prim.ScalarOp Prim.Lt))
  let lte = wrap_value (Prim (Prim.ScalarOp Prim.Lte))
  let gt = wrap_value (Prim (Prim.ScalarOp Prim.Gt))
  let gte = wrap_value (Prim (Prim.ScalarOp Prim.Gte))
  let eq = wrap_value (Prim (Prim.ScalarOp Prim.Eq))
  let neq = wrap_value (Prim (Prim.ScalarOp Prim.Neq))

  let plus = wrap_value (Prim (Prim.ScalarOp Prim.Add))
  let zero = wrap_value (Num (ParNum.zero Type.Int32T))
  let one = wrap_value (Num (ParNum.one Type.Int32T))
end
include ValueHelpers


module ExpHelpers = struct
  let call lhs args =
    { exp = Call(lhs, args);
      exp_src = None;
    }
end
include ExpHelpers

module StmtHelpers = struct
  let stmt ?src ?(id=StmtId.gen()) stmt =
  {
    stmt = stmt; stmt_src = src; stmt_id = id
  }

  let set ?src ids rhs =
  {
    stmt = Set(ids, rhs);
    stmt_src = src;
    stmt_id = StmtId.gen()
  }

  let setidx ?src lhs indices rhs =
  {
    stmt = SetIdx(lhs, indices, rhs);
    stmt_src = src;
    stmt_id = StmtId.gen()
  }

end
include StmtHelpers


module FnHelpers = struct
  let mk_fn ?name ~inputs ~input_names_to_ids ~output_ids ~body : fn =
    let fnId =
      match name with 
        | Some name -> FnId.gen_named name 
        | None -> FnId.gen()
    in
    {
      body = body;
      inputs = inputs;
      input_names_to_ids = input_names_to_ids; 
      output_ids = output_ids;
      fn_id = fnId;
    }

  let input_arity {inputs} = List.length inputs.Args.names
  let output_arity {output_ids} = List.length output_ids
  let fn_id {fn_id} = fn_id
end
include FnHelpers

module ScalarHelpers = struct
  let is_scalar_exp = function
    | Call({value=Prim (Prim.ScalarOp _)}, _ )
    | Values _ -> true
    | _ -> false

  let is_scalar_exp_node {exp} = is_scalar_exp exp

  let rec is_scalar_stmt ?(control_flow=false) = function
  | Set(_, expNode) -> is_scalar_exp_node expNode
  | If(_, tCode, fCode, _) ->
    control_flow && is_scalar_block tCode && is_scalar_block fCode
  | WhileLoop (condBlock, _, body, _) ->
    control_flow && is_scalar_block condBlock && is_scalar_block body
  | SetIdx _ -> false
  and is_scalar_stmt_node ?(control_flow=false) stmtNode =
    is_scalar_stmt ~control_flow stmtNode.stmt
  and is_scalar_block ?(control_flow=false) block =
    Block.for_all (is_scalar_stmt_node ~control_flow) block

  let is_scalar_fn ?(control_flow=false) fn =
    is_scalar_block ~control_flow fn.body
end
include ScalarHelpers
