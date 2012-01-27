open Base
open AST

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

(* Arr *)
let mk_arr_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) nodes =
  mk_node ~info ~src (Arr nodes)

let update_arr_node ast nodes = { ast with data = Arr nodes }

(* App *)
let mk_app_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) fnNode args =
  mk_node ~info ~src (App (fnNode, args))

let update_app_node ast fnNode args = { ast with data = App(fnNode, args)}

(* Prim *)
let mk_prim_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) p =
  mk_node ~info ~src (Prim p)

let update_prim_node ast p = {ast with data=Prim p}

(* Var *)
let mk_var_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) name =
  mk_node ~info ~src (Var name)

let update_var_node ast name = { ast with data = Var name }

(* Def *)
let mk_assign_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) lhs rhs =
  mk_node ~info ~src (Assign (lhs,rhs))

let update_assign_node ast lhs rhs = { ast with data = Assign(lhs, rhs) }

(* Lam *)
let mk_lam_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) args body =
  mk_node ~info  ~src  (Lam (args, body))

let update_lam_node ast args body = { ast with data = Lam(args, body) }

(* If *)
let mk_if_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty)
               condNode tNode fNode =
  mk_node ~info  ~src (If(condNode, tNode, fNode))

let update_if_node ast condNode tNode fNode =
  {ast with data=If(condNode,tNode,fNode)}

let mk_void_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) () =
  mk_node ~info ~src Void

let mk_iftrue_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty)
   condNode trueNodes =
  let voidNode = mk_void_node() in
  let blockNode = mk_block_node trueNodes in
  mk_node ~info ~src (If(condNode, blockNode, voidNode))

(* Int *)
let mk_int_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) x =
  mk_node ~info ~src $ Num (ParNum.coerce_int x Type.Int32T)

let update_int_node ast x =
  { ast with data = Num (ParNum.coerce_int x Type.Int32T) }

let mk_void_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) () =
  mk_node ~info ~src Void

(* Array indexing -- special case of App *)
let mk_idx_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) arrNode idx =
  mk_app_node ~info ~src arrNode [mk_int_node idx]

let mk_eq_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) lhsNode rhsNode =
  let eqOp = mk_node $ Prim (Prim.ScalarOp Prim.Eq) in
  mk_app_node ~info ~src eqOp [lhsNode; rhsNode]
(*
let mk_concat_node ?(info=mk_ast_info()) ?(src=SrcInfo.empty) lhs rhs =
  mk_app_node (mk_prim_node (Prim.ArrayOp Prim.Concat))  [lhs; rhs]
*)
(* given an AST lambda node, return the names of its arguments *)
let get_lam_args ast = match ast.data with
  | Lam(args, _ ) -> args
  | _ -> failwith "[get_lam_args] expected lambda node"

(* given an AST lambda node, return its body *)
let get_lam_body ast = match ast.data with
  | Lam(_, body) -> body
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
  | Assign(lhs,rhs) ->
    update_assign_node ast (flatten_block lhs) (flatten_block rhs)
  | _ -> ast

let rec is_void_recursive astNode = match astNode.data with
  | Void
  | Block [] -> true
  | Block nodes -> List.for_all is_void_recursive nodes
  | _ -> false
