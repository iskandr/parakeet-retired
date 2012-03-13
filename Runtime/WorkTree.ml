(* pp: -parser o pa_macro.cmo *)

open Base
open TypedSSA

type work_node = {
  adverb : adverb_info;
  input_shapes : Shape.t list;
  num_seq_stmts : int
}

type t = Node of work_node * work_node list (* node, children *)

let build_work_tree (fn:TypedSSA.fn) (args:values) =
  fn
