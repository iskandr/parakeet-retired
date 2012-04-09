open Base
open Prim
open Printf

module StrSet : module type of Set.Make(String)

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

val mk_ast_info : unit -> ast_info
val combine_return_arity : int option -> int option -> int option
val combine_ast_info : ast_info -> ast_info -> ast_info
val str_set_to_str : StrSet.t -> string
val info_to_str : ast_info -> string

type exp =
  | Var of string
  | Prim of Prim.t
  | Num of ParNum.t
  | Str of string
  | Type of Type.elt_t 
  | NoneVal
  | Array of node list
  | Tuple of node list
  | Lambda of node Args.formal_args * node
  | Call of node * node Args.actual_args 
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

val defs : node -> StrSet.t
val uses : node -> StrSet.t

val to_str : node -> string
val args_to_str : ?delim:string -> node list -> string

val print_ast_node : node -> unit
