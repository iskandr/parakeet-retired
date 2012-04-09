open Base
open Prim
open Printf

(*
  let min_elt set = fold min set (choose set)
  let max_elt set = fold max set (choose set)

  type set_comparison = Disjoint | Overlap | Same

  let compare_sets (set1 : 'a t) (set2 : 'a t) =
    let set3 = inter set1 set2 in
    if is_empty set3 then Disjoint
    else if (cardinal set3) = (cardinal set1) then Same
    else Overlap

*)
module StrSet = Set.Make(String)

type ast_info = {
  mutable defs_local : StrSet.t;
  mutable defs_global : StrSet.t;

  mutable reads_local : StrSet.t;
  mutable reads_global : StrSet.t;

  mutable writes_local : StrSet.t;
  mutable writes_global : StrSet.t;

  (* write to disk, print to screen, read from internet, etc... *)
  mutable io : bool;

  mutable nested_functions : bool;
  mutable is_function : bool;
  mutable return_arity : int option;
}

let mk_ast_info () = {
  defs_local = StrSet.empty;
  defs_global = StrSet.empty;
  reads_local = StrSet.empty;
  reads_global = StrSet.empty;
  writes_local = StrSet.empty;
  writes_global = StrSet.empty;
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
  defs_local = StrSet.union info1.defs_local info2.defs_local;
  defs_global = StrSet.union info1.defs_global info2.defs_global;

  reads_local = StrSet.union info1.reads_local info2.reads_local;
  reads_global = StrSet.union info1.reads_global info2.reads_global;

  writes_local = StrSet.union info1.writes_local info2.writes_local;
  writes_global = StrSet.union info1.writes_global info2.writes_global;

  io = info1.io || info2.io;
  nested_functions = info1.nested_functions || info2.nested_functions;
  is_function = info1.is_function || info2.is_function;
  return_arity = combine_return_arity info1.return_arity info2.return_arity;
}

let str_set_to_str set =
  "(" ^ (String.concat ", " (StrSet.elements set)) ^ ")"

let info_to_str info =
  sprintf
      "{reads_global: %s; writes_global: %s; io: %s; is_fn: %s; writes_local: %s}"
      (str_set_to_str info.reads_global)
      (str_set_to_str info.writes_global)
      (Bool.to_string info.io)
      (Bool.to_string info.is_function)
      (str_set_to_str info.writes_local)

type exp =
  | Var of string
  | Prim of Prim.t
  | Num of ParNum.t
  | Str of string
  | Type of Type.t 
  | NoneVal
  | Array of node list
  | Tuple of node list
  | Call of node * node Args.actual_args 
  | Lambda of node Args.formal_args * node
  | Assign of node list * node
  | Block of node list
  | Return of node list
  | If of node * node * node
  | WhileLoop of node * node
  | CountLoop of node * node

and node = {
  data:exp;
  src:SrcInfo.t;
  mutable ast_info : ast_info;
}

(* FIX: use a better AST_Info without all this local/global nonsense *)
let defs node =
  StrSet.union node.ast_info.defs_local node.ast_info.defs_global
let uses node =
  StrSet.union node.ast_info.reads_local node.ast_info.reads_global

let rec to_str node = match node.data with
  | Var name -> name
  | Prim p -> Prim.to_str p
  | Num (ParNum.Char c ) -> "'" ^ (Char.to_string c) ^ "'"
  | Num n -> ParNum.to_str n
  | Str str -> "\"" ^ (String.escaped str) ^ "\""
  | Type t -> "type(" ^ Type.to_str t ^ ")" 
  | NoneVal -> "None" 
  | Array elts -> "array([" ^ (args_to_str ~delim:", " elts) ^ "])"
  | Tuple elts -> "tuple(" ^ (args_to_str ~delim:", " elts) ^ ")" 
  | Lambda (formals, body) ->
    Base.indent_newlines $ 
      sprintf "lambda %s:\n%s" 
        (Args.formal_args_to_str ~value_to_str:to_str formals)
        (to_str body)
  | Call (fn, args) ->
    sprintf "%s(%s)" (to_str fn) (args_to_str ~delim:", " args)
  | Assign (lhsList, rhs) ->
    (args_to_str ~delim:", " lhsList) ^ " = " ^ (to_str rhs)
  | Block nodes -> 
    Base.indent_newlines $ 
      sprintf "\n%s" (args_to_str ~delim:"\n" nodes)
  | If(test, tNode, fNode) ->
    sprintf "if %s:\n%s\nelse:\n%s" (to_str test) 
    (Base.indent_newlines $ to_str tNode) 
    (Base.indent_newlines $ to_str fNode)
  | WhileLoop (test,body) ->
    sprintf "while %s:\n%s" 
      (to_str test)  
      (Base.indent_newlines (to_str body))
  | CountLoop (count, body) ->
    sprintf "repeat %s: %s" (to_str count) 
      (Base.indent_newlines (to_str body))
  | Return nodes -> sprintf "return %s" (args_to_str nodes)

and args_to_str ?(delim=", ") (args:node list) =
  String.concat delim (List.map to_str  args)

let print_ast_node n =
  Printf.printf "[AST] %s (source: %s) \n%!" (to_str n) (SrcInfo.to_str n.src)
