(* pp: -parser o pa_macro.cmo *)

open Base
open Printf


let rec wrap_str s =
  Str.global_substitute (Str.regexp "\n") (fun _ -> "  \n") s

type 'a phi_node = {
  phi_id : ID.t;
  phi_left:  'a;
  phi_right: 'a;
  phi_src : SrcInfo.t option;
}
type 'a phi_nodes = 'a phi_node list

let phi_node_to_str  value_node_to_str phiNode =
    sprintf "%s <- phi(%s, %s)"
      (ID.to_str phiNode.phi_id)
      (value_node_to_str phiNode.phi_left)
      (value_node_to_str phiNode.phi_right)

let phi_nodes_to_str  value_node_to_str  phiNodes =
  let lines = List.map (phi_node_to_str  value_node_to_str) phiNodes in
  String.concat "\n" lines

(* 'a = type of expressions *)
(* 'b = type of values *)
type ('a, 'b) stmt =
  | Set of ID.t list * 'a
  | SetIdx of 'b * 'b list * 'a
  | If of 'b * ('a, 'b) block * ('a, 'b) block * 'b phi_nodes
  (* testBlock, testVal, body, loop header, loop exit *)
  | WhileLoop of ('a, 'b) block * 'b * ('a, 'b) block * 'b phi_nodes
and ('a, 'b) stmt_node = {
  stmt: ('a, 'b) stmt;
  stmt_src: SrcInfo.t option;
  stmt_id : StmtId.t;
}
(* 'a = exp type, 'b = value type *)
and ('a,'b) block = ('a,'b) stmt_node Block.t

let ids_to_str (ids:ID.t list) = String.concat ", " (List.map ID.to_str ids)

let rec stmt_to_str
          (exp_to_str : 'a -> string)
          (val_to_str : 'b -> string)
          (stmt : ('a, 'b) stmt) =
  wrap_str $ match stmt with
  | Set (ids, rhs) -> sprintf "%s = %s" (ids_to_str ids) (exp_to_str rhs)
  | SetIdx (arr, indices, rhs) ->
    sprintf "%s[%s] = %s"
      (val_to_str arr)
      (String.concat ", " (List.map val_to_str indices))
      (exp_to_str rhs)
  | If (cond, tBlock, fBlock, phiNodes) ->
    wrap_str $ sprintf "if %s:\n%s \nelse:\n%s \nmerge:\n%s"
      (val_to_str cond)
      (block_to_str exp_to_str val_to_str tBlock)
      (block_to_str exp_to_str val_to_str fBlock)
      (phi_nodes_to_str val_to_str phiNodes)
  | WhileLoop (testBlock, testVal, body, phiNodes) ->
    wrap_str $ sprintf "while %s:\nloop header:\n%s\ndo:\n%s\nmerge:\n%s"
      (block_to_str exp_to_str val_to_str testBlock)
      (val_to_str testVal)
      (block_to_str exp_to_str val_to_str body)
      (phi_nodes_to_str val_to_str phiNodes)
and stmt_node_to_str exp_to_str val_to_str {stmt} =
  stmt_to_str exp_to_str val_to_str stmt
and block_to_str exp_to_str val_to_str block =
  Block.to_str (stmt_node_to_str exp_to_str val_to_str) block

(* search through a block and return the first srcinfo, *)
(* if one exists. Return None otherwise *)
let rec get_stmt_src_info {stmt; stmt_src} =
  if stmt_src <> None then stmt_src
  else match stmt with
    | If(_, tBlock, fBlock, _) ->
      let tSrc = get_block_src_info tBlock in
      if tSrc = None then get_block_src_info fBlock
      else tSrc
    | WhileLoop(condBlock, _, body, _) ->
      let condSrc = get_block_src_info condBlock in
      if condSrc = None then get_block_src_info body
      else condSrc
    | _ -> None
and get_block_src_info block = Block.find_first get_stmt_src_info block


module Untyped = struct
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

  type untyped_block = (exp_node, value_node) block

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

  let find_fn_src_info {body} = get_block_src_info body

  let input_arity {input_ids} = List.length input_ids
  let output_arity {output_ids} = List.length output_ids
  let fn_id {fn_id} = fn_id
end

module Typed = struct
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
end


