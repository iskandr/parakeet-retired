open Base
open Printf
open Prim

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
}

val mk_ast_info : unit -> ast_info
val combine_ast_info : ast_info -> ast_info -> ast_info
val str_set_to_str : string PSet.t -> string
val info_to_str : ast_info -> string

type exp =
    | Lam of (string list) * node
    | Var of string
    | Prim of prim
    | Num of ParNum.t
    | Str of string
    | Sym of string

    | App of node * node list
    | Arr of node list
    | If of node * node * node
    | Assign of node * node
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

val defs : node -> string PSet.t
val uses : node -> string PSet.t



val to_str : node -> string
val args_to_str : ?delim:string -> node list -> string