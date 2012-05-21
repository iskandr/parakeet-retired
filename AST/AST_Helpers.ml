open Base
open AST

let mk_formal_args names keywords keyword_values = 
  { Args.names = names; defaults = List.combine keywords keyword_values }
  
let mk_actual_args args keywords keyword_values = 
  { Args.values = args; keywords = List.combine keywords keyword_values } 

let mk_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) data =
  {data=data; src=src; ast_info = info}

(* Str *)
let mk_str_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) s =
  mk_node ~info ~src (Str s)

let update_str_node ast s = { ast with data = Str s }

(* Block *)
let mk_block_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) nodes =
  mk_node ~info ~src (Block nodes)

let update_block_node ast nodes = { ast with data = Block nodes }

(* Array *)
let mk_array_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) nodes =
  mk_node ~info ~src (Array nodes)

let update_array_node ast nodes = { ast with data = Array nodes }

(* Call *)
let mk_call_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) fnNode args =
  mk_node ~info ~src (Call (fnNode, args))

let update_call_node ast fnNode args = { ast with data = Call(fnNode, args)}

(* Prim *)
let mk_prim_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) p =
  mk_node ~info ~src (Prim p)

let update_prim_node ast p = {ast with data=Prim p}

(* Var *)
let mk_var_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) name =
  mk_node ~info ~src (Var name)

let update_var_node ast name = { ast with data = Var name }

(* Def *)
let mk_assign_node
      ?(info=mk_ast_info())
      ?(src=SrcInfo.empty)
      (lhs:AST.node list) rhs =
  mk_node ~info ~src (Assign (lhs,rhs))

let update_assign_node ast (lhs:AST.node list) rhs =
  { ast with data = Assign(lhs, rhs) }

(* Lambda *)
let mk_lambda_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) args body =
  mk_node ~info  ~src  (Lambda (args, body))

let update_lambda_node ast args body = { ast with data = Lambda(args, body) }

(* If *)
let mk_if_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty)
               condNode tNode fNode =
  mk_node ~info  ~src (If(condNode, tNode, fNode))

let update_if_node ast condNode tNode fNode =
  {ast with data=If(condNode,tNode,fNode)}

let mk_none_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) () =
  mk_node ~info ~src (NoneVal())

let mk_iftrue_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty)
   condNode trueNodes =
  let voidNode = mk_none_node() in
  let blockNode = mk_block_node trueNodes in
  mk_node ~info ~src (If(condNode, blockNode, voidNode))

(* Int *)
let mk_int_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) x =
  mk_node ~info ~src $ Num (ParNum.coerce_int x Type.Int32T)

let update_int_node ast x =
  { ast with data = Num (ParNum.coerce_int x Type.Int32T) }


(* Arrayay indexing -- special case of Call *)
let mk_idx_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) arrNode idx =
  mk_call_node ~info ~src arrNode $ Args.of_values [mk_int_node idx]

let mk_eq_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) lhsNode rhsNode =
  let eqOp = mk_node $ Prim (Prim.ScalarOp Prim.Eq) in
  mk_call_node ~info ~src eqOp $ Args.of_values [lhsNode; rhsNode]
(*
let mk_concat_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) lhs rhs =
  mk_call_node (mk_prim_node (Prim.ArrayayOp Prim.Concat))  [lhs; rhs]
*)
(* given an AST lambda node, return the names of its arguments *)
let get_lambda_args ast = match ast.data with
  | Lambda(args, _ ) -> args
  | _ -> failwith "[get_lam_args] expected lambda node"

(* given an AST lambda node, return its body *)
let get_lambda_body ast = match ast.data with
  | Lambda(_, body) -> body
  | _ -> failwith "[get_lam_body] expected lambda node"


(* get ride of nested blocks *)
let rec flatten_block ast = match ast.data with
  | Block nodes ->
    let f acc currNode =
      match currNode.data with
      | Block innerNodes -> acc @ innerNodes
      | _ -> acc @ [currNode]
    in
    let nodes' = List.fold_left f (List.map flatten_block nodes) [] in
    update_block_node ast nodes'
  | Assign(lhsList,rhs) ->
    let lhsList' = List.map flatten_block lhsList in
    update_assign_node ast (lhsList') (flatten_block rhs)
  | _ -> ast

let rec is_void_recursive astNode = match astNode.data with
  | NoneVal ()
  | Block [] -> true
  | Block nodes -> List.for_all is_void_recursive nodes
  | _ -> false
