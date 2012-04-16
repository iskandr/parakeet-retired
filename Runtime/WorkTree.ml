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
  num_scalar_ops : int;
  seq_cost : int;
  mutable parallel : bool
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
             nested_adverbs=[]; num_scalar_ops=0; seq_cost=0; parallel=false}
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

let get_tree_cost workTree costs =
  match workTree.adverb with
  | Some a -> (
    match a.Adverb.adverb with
    | Adverb.Map
    | Adverb.Reduce
    | Adverb.Scan ->
      let axes = List.map get_const_int a.Adverb.axes in
          let max_shape =
            List.fold_left
              (fun a b -> if Shape.rank a >= List.length axes then a else b)
              Shape.scalar_shape workTree.arg_shapes
          in
          let nelts =
        List.fold_left
          (fun acc axis -> acc * (Shape.get max_shape axis)) 1 axes
      in
      let seq =
        List.fold_left (fun a b -> a + b) workTree.num_scalar_ops costs
      in
      if workTree.parallel then
        seq * nelts / MachineModel.num_hw_threads
      else
        seq * nelts
    | Adverb.AllPairs ->
      assert(List.length a.Adverb.axes = 1);
      let axis = get_const_int (List.hd a.Adverb.axes) in
      let iter_cost =
        List.fold_left (fun a b -> a + b) workTree.num_scalar_ops costs
      in
      let seq_cost =
        List.fold_left
          (fun acc s -> acc * (Shape.get s axis)) iter_cost workTree.arg_shapes
      in
      if workTree.parallel then
        seq_cost / MachineModel.num_hw_threads
      else
        seq_cost
  )
  | None ->
    List.fold_left (fun a b -> a + b) workTree.num_scalar_ops costs

let rec fill_in_seq_costs workTree =
  match workTree.nested_adverbs with
  | [] ->
    let nelts = List.map Shape.nelts workTree.arg_shapes in
    let cost =
      List.fold_left (fun a b -> a * b) 1 ([workTree.num_scalar_ops] @ nelts)
    in
    let newTree = {workTree with seq_cost=cost} in
    newTree, cost
  | _ :: _ ->
    let newChildren, costs =
      let tmp = List.map fill_in_seq_costs workTree.nested_adverbs in
      (List.map (fun (a,b) -> a) tmp), (List.map (fun (a,b) -> b) tmp)
    in
    let cost = get_tree_cost workTree costs in
    let newTree = {workTree with seq_cost=cost; nested_adverbs=newChildren} in
    newTree, cost

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
    num_scalar_ops=0;
    seq_cost=0;
    parallel=false
  }
  in
  fill_in_seq_costs (build_work_tree_from_shapes curTree fn argShapes)

let rec get_parallel_cost node =
  match node.nested_adverbs with
  | [] ->
    if node.parallel then
      node.seq_cost / MachineModel.num_hw_threads
    else
      node.seq_cost
  | _ :: _ ->
    let nested_costs = List.map get_parallel_cost node.nested_adverbs in
    let seq_cost = get_tree_cost node nested_costs in
    if node.parallel then
      seq_cost / MachineModel.num_hw_threads
    else
      seq_cost

let rec best_parallel_aux root node =
  match node.nested_adverbs with
  | [] ->
    node.parallel <- true;
    let cost = get_parallel_cost root in
    node.parallel <- false;
    cost, [node]
  | _ :: _ ->
    let nested_best_nodes =
      let costs_and_nodes =
        List.map (best_parallel_aux root) node.nested_adverbs
      in
      List.flatten (List.map (fun (a,b) -> b) costs_and_nodes)
    in
    List.iter (fun a -> a.parallel <- true) nested_best_nodes;
    let nested_cost = get_parallel_cost root in
    List.iter (fun a -> a.parallel <- false) nested_best_nodes;
    node.parallel <- true;
    let cur_cost = get_parallel_cost root in
    node.parallel <- false;
    if cur_cost < nested_cost then
      cur_cost, [node]
    else
      nested_cost, nested_best_nodes

let best_parallel tree =
  let cost, par_nodes = best_parallel_aux tree tree in
  IFDEF DEBUG THEN
  List.iter
    (fun n ->
      let id = match n.stmt_id with
        | Some id -> StmtId.to_str id
        | None -> "None"
      in
      Printf.printf "Best node with id: %s\n" id)
    par_nodes;
  ENDIF;
  par_nodes

let rec aux_to_str num_spaces tree =
  let id = match tree.stmt_id with
    | Some id -> StmtId.to_str id
    | None -> "None"
  in
  match tree.adverb with
  | Some adverb_info ->
    Printf.printf "%*s%s[id=%s](%d) : %s : %d\n%!"
      num_spaces
      ""
      (Adverb.to_str adverb_info.Adverb.adverb)
      id
      tree.num_scalar_ops
      (Shape.shape_list_to_str tree.arg_shapes)
      tree.seq_cost
    ;
    List.iter (aux_to_str (num_spaces + 2)) tree.nested_adverbs
  | None ->
    Printf.printf "WorkTreeRoot(%d)\n%!" tree.num_scalar_ops;
    List.iter (aux_to_str (num_spaces + 2)) tree.nested_adverbs

let to_str tree =
  aux_to_str 0 tree
