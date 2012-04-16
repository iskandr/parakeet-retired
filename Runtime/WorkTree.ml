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
  seq_cost : int
}

type annotation = Multicore | SSE | Tiling of int * int * int
type plan_t = (StmtId.t, (annotation list)) Hashtbl.t

let mc = Multicore
let sse = SSE
let tiling = Tiling(0,0,0)
let all_annotations = [mc (*;tiling*) (*;sse*)]

let rec powerset = function
  | [] -> [[]]
  | h::t -> List.fold_left (fun xs t -> (h::t)::t::xs) [] (powerset t)

let annotations_powerset = powerset all_annotations

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
             nested_adverbs=[]; num_scalar_ops=0; seq_cost=(-1)}
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

let empty_plan : plan_t = Hashtbl.create 1
let rec get_tree_cost ?plan:(p=empty_plan) workTree =
  let seq_cost =
    if workTree.seq_cost = -1 then
      let child_costs =
        match workTree.nested_adverbs with
        | [] -> []
        | h::t ->
          List.map
            (fun child -> get_tree_cost ~plan:p child)
            workTree.nested_adverbs
      in
      let scalar_cost =
        workTree.num_scalar_ops +
          (List.fold_left (fun a b -> a + b) 0 child_costs)
      in
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
          scalar_cost * nelts
        | Adverb.AllPairs ->
          assert(List.length a.Adverb.axes = 1);
          let axis = get_const_int (List.hd a.Adverb.axes) in
          List.fold_left
            (fun acc s -> acc * (Shape.get s axis))
            scalar_cost workTree.arg_shapes
      )
      | None -> scalar_cost
    else
      workTree.seq_cost
  in
  match workTree.stmt_id with
  | Some stmtId ->
    if Hashtbl.mem p stmtId then
      let annotations = Hashtbl.find p stmtId in
      if List.mem mc annotations then
        seq_cost / MachineModel.num_hw_threads
      else
        seq_cost
    else
      seq_cost
  | None -> seq_cost

let rec fill_in_seq_costs workTree =
  match workTree.nested_adverbs with
  | [] ->
    let nelts = List.map Shape.nelts workTree.arg_shapes in
    let cost =
      List.fold_left (fun a b -> a * b) 1 ([workTree.num_scalar_ops] @ nelts)
    in
    {workTree with seq_cost=cost}
  | _ :: _ ->
    let newChildren = List.map fill_in_seq_costs workTree.nested_adverbs in
    let cost = get_tree_cost {workTree with nested_adverbs=newChildren} in
    {workTree with seq_cost=cost; nested_adverbs=newChildren}

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
    seq_cost=(-1)
  }
  in
  fill_in_seq_costs (build_work_tree_from_shapes curTree fn argShapes)

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

module AnnSet =
  Set.Make(struct type t = annotation let compare = compare end)
let prune_plan_forest forest workTree =
  let rec is_valid_aux plan ancestor_anns cur_node =
    let stmtId = match cur_node.stmt_id with
      | Some id -> id
      | None -> failwith "Can't annotate root node of WorkTree"
    in
    let cur_anns =
      let empty_set = AnnSet.empty in
      let anns = Hashtbl.find plan stmtId in
      List.fold_left (fun s ann -> AnnSet.add ann s) empty_set anns
    in
    if not $ AnnSet.is_empty (AnnSet.inter ancestor_anns cur_anns) then
      false
    else
      let new_anns = (AnnSet.union ancestor_anns cur_anns) in
      let rec recurse = function
        | [] -> true
        | hd::tl ->
          if not (is_valid_aux plan new_anns hd) then
            false
          else
            recurse tl
      in
      recurse cur_node.nested_adverbs
  in
  let is_valid plan =
    let empty_set = AnnSet.empty in
    let rec recurse = function
      | [] -> true
      | hd::tl ->
        if not (is_valid_aux plan empty_set hd) then
          false
        else
          recurse tl
    in
    recurse workTree.nested_adverbs
  in
  List.filter is_valid forest

let rec build_plans_aux cur_plans cur_tree =
  let stmtId = match cur_tree.stmt_id with
    | Some id -> id
    | None -> failwith "Can't annotate root of workTree"
  in
  let sub_plans = match cur_tree.nested_adverbs with
    | [] ->
      cur_plans
    | _ :: _ ->
      List.fold_left build_plans_aux cur_plans cur_tree.nested_adverbs
  in
  let get_new_plan sub_plan annotations =
    let new_plan : plan_t = Hashtbl.copy sub_plan in
    Hashtbl.add new_plan stmtId annotations;
    new_plan
  in
  let get_new_plans sub_plan =
    List.map (get_new_plan sub_plan) annotations_powerset
  in
  List.flatten $ List.map get_new_plans sub_plans

let build_plans workTree =
  let plan : plan_t = Hashtbl.create 127 in
  let all_plans = match workTree.nested_adverbs with
    | [] -> [plan]
    | _ :: _ ->
      List.fold_left build_plans_aux [plan] workTree.nested_adverbs
  in
  Printf.printf "Number of plans generated: %d\n%!" (List.length all_plans);
  let plans = prune_plan_forest all_plans workTree in
  Printf.printf "Number of valid plans found: %d\n%!" (List.length plans);
  plans

let annotations_to_str annotations =
  List.fold_left
    (fun cur ann ->
      cur ^ (
      match ann with
      | Multicore -> "Multicore"
      | SSE -> "SSE"
      | Tiling(x,y,z) -> Printf.sprintf "Tiling(%d,%d,%d)" x y z))
    "" annotations

let rec plan_to_str_aux num_spaces plan tree =
  let id, annotations = match tree.stmt_id with
    | Some id ->
      (StmtId.to_str id), (annotations_to_str (Hashtbl.find plan id))
    | None -> "None", "None"
  in
  match tree.adverb with
  | Some adverb_info ->
    Printf.printf "%*s%s[id=%s][%s](%d) : %s : %d\n%!"
      num_spaces
      ""
      (Adverb.to_str adverb_info.Adverb.adverb)
      id
      annotations
      tree.num_scalar_ops
      (Shape.shape_list_to_str tree.arg_shapes)
      (get_tree_cost ~plan:plan tree)
    ;
    List.iter (plan_to_str_aux (num_spaces + 2) plan) tree.nested_adverbs
  | None ->
    Printf.printf "WorkTreeRoot(%d)\n%!" tree.num_scalar_ops;
    List.iter (plan_to_str_aux (num_spaces + 2) plan) tree.nested_adverbs

let plan_to_str plan tree =
  plan_to_str_aux 0 plan tree

let best_plan workTree =
  let plans = build_plans workTree in
  let (plan, cost) = List.fold_left
    (fun (cur_plan, cur_cost) new_plan ->
      let new_cost = get_tree_cost ~plan:new_plan workTree in
      if new_cost < cur_cost then
        (new_plan, new_cost)
      else
        (cur_plan, cur_cost))
    (empty_plan, max_int) plans
  in
  plan_to_str plan workTree;
  plan
