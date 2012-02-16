open Base
open Prim
open Printf

type ast_info = {
	mutable defs_local : string PSet.t;
	mutable defs_global : string PSet.t;

	mutable reads_local : string PSet.t;
	mutable reads_global : string PSet.t;

	mutable writes_local : string PSet.t;
	mutable writes_global : string PSet.t;

	(* write to disk, print to screen, read from internet, etc... *)
	mutable io : bool;

	mutable nested_functions : bool;
	mutable is_function : bool;
	mutable return_arity : int option;
}

let mk_ast_info () = {
	defs_local = PSet.empty;
	defs_global = PSet.empty;
	reads_local = PSet.empty;
	reads_global = PSet.empty;
	writes_local = PSet.empty;
	writes_global = PSet.empty;
	io = false;
	nested_functions = false;
	is_function = false;
	return_arity = None;
}

let combine_return_arity r1 r2 =
  match r1, r2 with
    | None, None -> None
    | Some x, None -> Some x
    | None, Some y -> Some y
    | Some x, Some y ->
      if x = y then Some x else failwith "Return arity mismatch"

let combine_ast_info info1 info2 = {
	defs_local = PSet.union info1.defs_local info2.defs_local;
	defs_global = PSet.union info1.defs_global info2.defs_global;

	reads_local = PSet.union info1.reads_local info2.reads_local;
	reads_global = PSet.union info1.reads_global info2.reads_global;

	writes_local = PSet.union info1.writes_local info2.writes_local;
	writes_global = PSet.union info1.writes_global info2.writes_global;

	io = info1.io || info2.io;
	nested_functions = info1.nested_functions || info2.nested_functions;
	is_function = info1.is_function || info2.is_function;
	return_arity = combine_return_arity info1.return_arity info2.return_arity;
}

let str_set_to_str set =
  "(" ^ (String.concat ", " (PSet.elements set)) ^ ")"

let info_to_str info =
	sprintf
      "{reads_global: %s; writes_global: %s; io: %s; is_fn: %s; writes_local: %s}"
      (str_set_to_str info.reads_global)
      (str_set_to_str info.writes_global)
      (Bool.to_string info.io)
      (Bool.to_string info.is_function)
      (str_set_to_str info.writes_local)

(* core lambda language + random junk for side effects *)
type exp =
	| Lam of (string list) * node
	| Var of string
	| Prim of Prim.t
	| Num of ParNum.t
	| Str of string
	| App of node * node list
	| Arr of node list
	| If of node * node * node
	| Assign of node list * node
	| Block of node list
	| WhileLoop of node * node
	| CountLoop of node * node
	| Return of node list
	| Void

and node = {
	data:exp;
	src:SrcInfo.t;
	mutable ast_info : ast_info;
}

(* FIX: use a better AST_Info without all this local/global nonsense *)
let defs node =
  PSet.union node.ast_info.defs_local node.ast_info.defs_global
let uses node =
  PSet.union node.ast_info.reads_local node.ast_info.reads_global

let rec to_str ast = match ast.data with
  | Lam (ids, body) ->
        sprintf "fn(%s) {%s}" (String.concat "; " ids) (to_str body)
  | Var name -> name
  | Prim p -> Prim.to_str p
  | Num (ParNum.Char c ) -> "'" ^ (Char.to_string c) ^ "'"
  | Num n -> ParNum.to_str n
  | Str str -> "\"" ^ (String.escaped str) ^ "\""
  | App (fn, args) ->
    sprintf "%s(%s)" (to_str fn) (args_to_str ~delim:", " args)
  | Arr elts -> "[" ^ (args_to_str ~delim:", " elts) ^ "]"
  | Assign (lhsList, rhs) ->
    (args_to_str ~delim:", " lhsList) ^ " := " ^ (to_str rhs)
  | Block nodes -> sprintf "{ %s }" (args_to_str nodes)
  | If(test, tNode, fNode) ->
    sprintf "if %s then %s else %s" (to_str test) (to_str tNode) (to_str fNode)
  | Void -> "void"
  | WhileLoop (test,body) ->
    sprintf "while %s do %s" (to_str test)  (to_str body)
  | CountLoop (count, body) ->
    sprintf "repeat %s do %s" (to_str count) (to_str body)
  | Return nodes -> sprintf "return %s" (args_to_str nodes)

and args_to_str ?(delim="; ") (args:node list) =
  String.concat delim (List.map to_str  args)

let print_ast_node n =
  Printf.printf "[AST] %s (source: %s) \n%!" (to_str n) (SrcInfo.to_str n.src)
