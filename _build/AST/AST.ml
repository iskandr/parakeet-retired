open Base
open PQNum
open SourceInfo
open Printf
open Prim 

include AST_Info

(* core lambda language + random junk for side effects *)  
type exp = 
    | Lam of (string list) * node
    | Var of string
    | Prim of prim
    | Num of num 
    | Str of string
    | Sym of string
				
    | App of  node * node list 
    | Arr of node list 
    | If of node * node * node
    | Def of string * node
    | SetIdx of string * (node list) * node 
				
    | Block of node list
    | WhileLoop of node * node
    | CountLoop of node * node 
    | Void
      
and node = { 
    data:exp; 
    src:source_info;
		mutable ast_info : ast_info; 
}

let mk_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) data  = 
  {data=data; src=src; ast_info = astInfo}

(* Str *) 
let mk_str_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) s =
  mk_node ~astInfo ~src (Str s) 

let update_str_node ast s = { ast with data = Str s }

(* Block *) 
let mk_block_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) nodes = 
  mk_node ~astInfo ~src (Block nodes)

let update_block_node ast nodes = { ast with data = Block nodes } 
     
(* Arr *)     
let mk_arr_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) nodes = 
  mk_node ~astInfo ~src (Arr nodes) 

let update_arr_node ast nodes = { ast with data = Arr nodes } 

(* App *) 
let mk_app_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) fnNode args = 
  mk_node ~astInfo ~src (App (fnNode, args)) 

let update_app_node ast fnNode args = { ast with data = App(fnNode, args)}

(* Prim *) 
let mk_prim_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) p = 
  mk_node ~astInfo ~src (Prim p) 
  
let update_prim_node ast p = {ast with data=Prim p} 

(* Var *) 
let mk_var_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) name = 
  mk_node ~astInfo ~src (Var name) 

let update_var_node ast name = { ast with data = Var name } 

(* Def *)   
let mk_def_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) name rhs = 
  mk_node ~astInfo ~src (Def (name,rhs)) 

let update_def_node ast name rhs = { ast with data = Def(name, rhs) }

(* Lam *) 
let mk_lam_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) args body = 
  mk_node ~astInfo  ~src  (Lam (args, body))

let update_lam_node ast args body = { ast with data = Lam(args, body) } 
    
(* If *)     
let mk_if_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) 
               condNode tNode fNode = 
  mk_node ~astInfo  ~src (If(condNode, tNode, fNode))

let update_if_node ast condNode tNode fNode = 
  {ast with data=If(condNode,tNode,fNode)}

let mk_void_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) () = 
  mk_node ~astInfo ~src Void 
  
let mk_iftrue_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) 
   condNode trueNodes =
  let voidNode = mk_void_node() in 
  let blockNode = mk_block_node trueNodes in 
  mk_node ~astInfo ~src (If(condNode, blockNode, voidNode)) 

(* Int *) 
let mk_int_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) x = 
  mk_node ~astInfo ~src $ Num (Int x)  

let update_int_node ast x = {ast with data = Num(Int x) }

let mk_void_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) () = 
  mk_node ~astInfo ~src Void 

(* Array indexing -- special case of App *) 
let mk_idx_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) arrNode idx = 
  mk_app_node ~astInfo ~src arrNode [mk_int_node idx]

let mk_eq_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) lhsNode rhsNode = 
  let eqOp = mk_node $ Prim (Prim.ScalarOp Prim.Eq) in 
  mk_app_node ~astInfo ~src eqOp [lhsNode; rhsNode]

let mk_concat_node ?(astInfo=mk_ast_info()) ?(src=emptySourceInfo) lhs rhs = 
  mk_app_node (mk_prim_node (Prim.ArrayOp Prim.Concat))  [lhs; rhs]

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
  | Def(lhs,rhs) -> update_def_node ast lhs (flatten_block rhs) 
  | _ -> ast   


let rec node_to_str ?(inBlock=false) node = 
  match node.data with
  (*| Lam(ids, {data=Block nodes}) -> 
     sprintf "{[%s] %s}" 
      (String.concat "; " ids) 
      (args_to_str ~delim:"; " nodes)
  *)
  | Lam (ids, body) -> 
     sprintf "{[%s] %s}" 
       (String.concat "; " ids) (node_to_str ~inBlock:false body)
  | Var name -> name
  | Prim p -> prim_to_str p
  | Sym name -> "`"^name 
  | Num (Char c ) -> "\"" ^ (Char.to_string c) ^ "\""
  | Num (Float32 f) -> Float.to_string f ^ "e"
  | Num n -> num_to_str n 
  | Str str -> "\"" ^ (String.escaped str)  ^ "\""
  | App ({data=Prim (ArrayOp p)}, [fnArg; arg1; arg2])
  | App ({data=App({data=Prim (ArrayOp p)}, [])}, [fnArg; arg1; arg2])
  | App ({data=App({data=Prim (ArrayOp p)}, [fnArg])}, [arg1; arg2])
    when Prim.is_adverb p  -> 
      sprintf "%s %s%s %s"
        (node_to_str arg1)
        (node_to_str fnArg)
        (Prim.array_op_to_str p)
        (node_to_str arg2)
  | App(fn,args) -> 
        (node_to_str ~inBlock:true fn)^ 
        "[" ^ (args_to_str  args)  ^"]"
  | Arr [] -> "()"
  | Arr [elt] -> "(enlist " ^ (node_to_str ~inBlock:true elt) ^ ")"
  | Arr elts ->   "(" ^ args_to_str elts ^ ")"
  | Def (name, rhs) -> name ^": "^(node_to_str ~inBlock:true rhs) 
  | SetIdx (name, indices, rhs) -> 
	  name ^ "[" ^ (args_to_str indices) ^ "]: "^ (node_to_str ~inBlock rhs) 
  | Block nodes ->
    let left = if inBlock then "[" else "" in 
    let right = if inBlock then "]" else "" in    
	  left ^ (args_to_str ~delim:";\n  "  nodes) ^  right 
    (* special case for if statements which only do something
       on the true branch 
    *) 
  | If(test, tNode, fNode) ->
      let testStr = node_to_str test  in 
      let tStr = node_to_str ~inBlock:true tNode in 
      let fStr = node_to_str ~inBlock:true fNode in 
      sprintf "$[%s;%s;%s]" testStr tStr fStr 
  | Void -> "::"
  | _ -> "<no representation>"

and args_to_str ?(delim="; ") args = 
    String.concat delim (List.map (node_to_str ~inBlock:true) args)
