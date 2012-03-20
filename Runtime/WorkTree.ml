(* pp: -parser o pa_macro.cmo *)

open Adverb
open Base
open SSA_Analysis
open TypedSSA

type t = {
  adverb : adverb_info option;
  stmt_id : StmtId.t option;
  arg_shapes : Shape.t list;
  nested_adverbs : t list;
  num_scalar_ops : int
}

module type WORKTREE_PARAMS = sig
  val cur_tree : t
  val shapes : Shape.t list
  val build_work_tree : t -> TypedSSA.fn -> Shape.t list -> t
end
module WorkTreeArgs(P: WORKTREE_PARAMS) = struct
  type env = t
  type value_info = unit
  type exp_info = unit

  let dir = Forward
  let iterative = false

  let init fn = P.cur_tree

  let value _ _ = ()
  let exp _ _ _ = ()

  let phi_set _ _ _ = None
  let phi_merge set id _ _ = None

  let stmt tree stmtNode helpers =
    match stmtNode.stmt with
    | Set(_, expNode)
    | SetIdx(_, _, expNode) ->
      begin match expNode.exp with
        | Adverb info ->
          let id = stmtNode.TypedSSA.stmt_id in
          let axes = List.map get_const_int info.axes in
          let nestedFn = FnManager.get_typed_function info.adverb_fn in
          let child_node_empty =
            {adverb=Some info; stmt_id=Some id; arg_shapes=P.shapes;
             nested_adverbs=[]; num_scalar_ops=0}
          in
          let newShapes = List.map (Shape.peel ~axes) P.shapes in
          let child_node =
            P.build_work_tree child_node_empty nestedFn newShapes
          in
          let nested_adverbs = tree.nested_adverbs @ [child_node] in
          Some {tree with nested_adverbs=nested_adverbs}
        | _ ->
          let num_scalar_ops = tree.num_scalar_ops + 1 in
          Some {tree with num_scalar_ops=num_scalar_ops}
      end
    | If(_, _, _, _)
    | WhileLoop(_, _, _, _) ->
      helpers.eval_stmt tree stmtNode
end

let rec build_work_tree_from_shapes curTree fn shapes =
  let module Params = struct
    let cur_tree = curTree
    let shapes = shapes
    let build_work_tree = build_work_tree_from_shapes
  end
  in
  let module WorkTreeBuilder = SSA_Analysis.MkEvaluator(WorkTreeArgs(Params)) in
  WorkTreeBuilder.eval_fn fn

let build_work_tree fn args =
  let argShapes = List.map Value.get_shape args in
  let curTree = {
    adverb=None;
    stmt_id=None;
    arg_shapes=[];
    nested_adverbs=[];
    num_scalar_ops=0
  }
  in
  build_work_tree_from_shapes curTree fn argShapes

let rec aux_to_str num_spaces tree =
  (*Printf.printf "------------ %d \n%!" num_spaces;*)
  match tree.adverb with
  | Some adverb_info ->
    Printf.printf "%*s%s(%d) : %s\n%!"
      num_spaces
      ""
      (Adverb.to_str adverb_info.Adverb.adverb)
      tree.num_scalar_ops
      (Shape.shape_list_to_str tree.arg_shapes)
    ;
    List.iter (aux_to_str (num_spaces + 2)) tree.nested_adverbs
  | None ->
    Printf.printf "WorkTreeRoot(%d)\n%!" tree.num_scalar_ops;
    List.iter (aux_to_str (num_spaces + 2)) tree.nested_adverbs

let to_str tree =
  aux_to_str 0 tree
