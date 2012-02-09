(* pp: -parser o pa_macro.cmo *)

open Base

type value =
  | Var of ID.t
  | Num of ParNum.t
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
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
  (*closure_input_types:Type.t list;
  closure_output_types: Type.t list
  *)
}

type adverb_args = {
  axes : int list;
  init : value_nodes option;
  args : value_nodes
}

type exp =
  (* application of arbitrary values used only in untyped code *)
  | App of  value_node * value_nodes
  (* construction of arrays and values used by both typed and untyped ssa *)
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *)
  | Cast of Type.t * value_node
  | Call of FnId.t * value_nodes
  | PrimApp of Prim.prim * value_nodes
  | Adverb of Prim.adverb * closure * adverb_args


and exp_node = {
  exp: exp;
  exp_src : SrcInfo.t option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)
  exp_types : Type.t list;
}

type stmt =
  | Set of ID.t list * exp_node
  | SetIdx of value_node * value_nodes * value_node
  | If of value_node * block * block * phi_nodes
  (* testBlock, testVal, body, loop header, loop exit *)
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
  input_ids: ID.t list;
  output_ids: ID.t list;
  fn_input_types : Type.t list;
  fn_output_types : Type.t list;
  fn_id : FnId.t;
}
and tenv = Type.t ID.Map.t


let is_simple_exp = function
  | Call _
  | PrimApp _
  | Adverb _
  | App _ -> false
  | _ -> true



open Printf

let rec id_list_to_str = function
  | [] -> ""
  | [id] -> ID.to_str id
  | id::rest -> (ID.to_str id) ^ ", " ^ (id_list_to_str rest)

let rec typed_id_to_str tenv id =
  if ID.Map.mem id tenv then
    sprintf "%s : %s" (ID.to_str id) (Type.to_str (ID.Map.find id tenv))
  else sprintf "%s" (ID.to_str id)

let rec typed_id_list_to_str tenv = function
  | [] -> ""
  | [id] -> typed_id_to_str tenv id
  | id::rest ->
    sprintf "%s, %s" (typed_id_to_str tenv id) (typed_id_list_to_str tenv rest)

let value_to_str = function
  | GlobalFn fnId -> FnId.to_str fnId
  | Var id -> ID.to_str id
  | Num n -> ParNum.to_str n
  | Str s -> "\""^s ^"\""
  | Sym s -> "`" ^ s
  | Unit -> "()"
  | Prim p -> "PRIM(" ^ (Prim.to_str p) ^ ")"

let value_node_to_str vNode =
  let valStr = value_to_str vNode.value in
  if vNode.value_type <> Type.BottomT then
    sprintf "%s : %s" valStr (Type.to_str vNode.value_type)
  else valStr

let rec value_nodes_to_str = function
  | [] -> ""
  | [v] -> value_node_to_str v
  | v::vs -> (value_node_to_str v) ^ "; " ^ (value_nodes_to_str vs)
and value_node_list_to_str ?(sep=",") vs =
  String.concat (sep^" ") (List.map value_node_to_str vs)

let value_list_to_str ?(sep=",") vs =
  String.concat (sep^" ") (List.map value_to_str vs)

let closure_to_str cl =
   (FnId.to_str cl.closure_fn) ^
   (if List.length cl.closure_args > 0 then
      " @ " ^ (value_nodes_to_str cl.closure_args)
    else "")

let adverb_args_to_str {init; axes; args} =
  Printf.sprintf "%s, axes=%s, init=%s"
    (value_nodes_to_str args)
    (String.concat ", " (List.map string_of_int axes))
    (match init with
      | None -> "none"
      | Some initvals -> (value_nodes_to_str initvals)
    )

let exp_to_str expNode =
  match expNode.exp with
  | Values vs -> value_nodes_to_str vs
  | App ({value=Prim op}, args) ->
    sprintf "%s(%s) {{untyped}}" (Prim.to_str op) (value_node_list_to_str args)
  | App (fn, args) ->
    sprintf "%s(%s)" (value_node_to_str fn)  (value_node_list_to_str args)
  | Arr elts -> "[" ^ (value_node_list_to_str ~sep:";" elts) ^ "]"
  | Cast(t, v) ->
      sprintf "cast<%s>(%s)" (Type.to_str t) (value_node_to_str v)

  | Call (fnId, args) ->
      sprintf "%s(%s)" (FnId.to_str fnId) (value_nodes_to_str args)
  | PrimApp (p, args) ->
      sprintf "prim{%s}(%s)"
        (Prim.to_str p)
        (value_nodes_to_str args)
  | Adverb(adverb, closure, adverb_args) ->
      sprintf
        "%s[%s](%s)"
        (Prim.adverb_to_str adverb)
        (closure_to_str closure)
        (adverb_args_to_str adverb_args)


let phi_node_to_str ?(space="") phiNode =
  Printf.sprintf "%s%s : %s <- phi(%s, %s)"
    space
    (ID.to_str phiNode.phi_id)
    (Type.to_str phiNode.phi_type)
    (value_node_to_str phiNode.phi_left)
    (value_node_to_str phiNode.phi_right)

let phi_nodes_to_str ?(space="") phiNodes =
  String.concat "\n" (List.map (phi_node_to_str ~space) phiNodes)

let rec block_to_str ?(space="") ?(tenv=ID.Map.empty) block =
  Block.to_str (stmt_node_to_str ~space ~tenv) block
and stmt_node_to_str ?(space="") ?(tenv=ID.Map.empty) stmtNode =
  let str = match stmtNode.stmt with
  | Set (ids, rhs) ->
    sprintf "%s = %s " (typed_id_list_to_str tenv ids) (exp_to_str rhs)
  | SetIdx (lhs, indices, rhs) ->
    sprintf "%s[%s] = %s"
      (value_node_to_str lhs)
      (value_nodes_to_str indices)
      (value_node_to_str rhs)
  | If (cond,tBlock,fBlock, phiNodes) ->
      let space' =  "\t"^space in
      let tStr =
        Printf.sprintf "%s  <Then>:\n%s"
          space
          (block_to_str ~space:space' ~tenv tBlock)
      in
      let fStr =
        Printf.sprintf "%s  <Else>:\n%s"
          space
          (block_to_str ~space:space' ~tenv fBlock)
      in
      let mergeStr =
        Printf.sprintf "%s  <Merge>:\n%s"
          space
          (phi_nodes_to_str ~space:space' phiNodes)
      in
      Printf.sprintf "If (%s)\n%s\n%s\n%s"
         (value_node_to_str cond) tStr fStr mergeStr

  | WhileLoop (testBlock, testVal, body, header) ->
      let space' =  "\t"^space in
      let headerStr =
        Printf.sprintf "%s  <Header>:\n%s"
          space
          (phi_nodes_to_str ~space:space' header)
      in

      let loopTestStr =
        Printf.sprintf "%s  <TestBlock>:%s\n%s  <TestVal>: %s"
          space
          (block_to_str ~space:space' ~tenv testBlock)
          space
          (value_node_to_str testVal)
      in
      let bodyStr =
        Printf.sprintf "%s  <Body>:%s"
          space
          (block_to_str ~space:space' ~tenv body)
      in

      Printf.sprintf "while\n%s\n%s\n%s\n"
        headerStr
        loopTestStr
        bodyStr
  in space ^ str

let fn_to_str (fundef:fn) =
  let name = FnId.to_str fundef.fn_id in
  let inputs = typed_id_list_to_str fundef.tenv fundef.input_ids in
  let outputs = typed_id_list_to_str fundef.tenv fundef.output_ids in
  let body = block_to_str ~space:"\t" ~tenv:fundef.tenv fundef.body in
  sprintf "%s (%s)=>(%s) { \n %s \n }" name inputs outputs body



