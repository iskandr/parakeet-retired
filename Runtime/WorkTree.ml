(* pp: -parser o pa_macro.cmo *)

open Base
open SSA_Analysis
open TypedSSA

type t = {
  adverb : adverb_info option;
  stmt_id : StmtId.t option;
  nested_adverbs : t list;
  num_scalar_ops : int
}

module WorkTreeBuilder = SSA_Analysis.MkEvaluator(struct
  type env = t
  type value_info = unit
  type exp_info = unit

  let dir = Forward
  let iterative = false

  let init fn = {adverb=None; stmt_id=None; nested_adverbs=[]; num_scalar_ops=0}

  let value _ _ = ()
  let exp _ _ _ = ()

  let phi_set _ _ _ = None
  let phi_merge set id _ _ = None

  let stmt tree stmtNode helpers =
    match stmtNode.stmt with
    | Set(_, expNode)
    | SetIdx(_, _, expNode) ->
      begin match expNode.exp with
        | Adverb adverb_info ->
          let child_node =
            {adverb=adverb_info; stmt_id=stmtNode.stmt_id;
             nested_adverbs=[]; num_scalar_ops=0}
          in
          let child = helpers.iter_exp_children child_node exp in
          let nested_adverbs = tree.nested_adverbs @ [child] in
          {tree with nested_adverbs=nested_adverbs}
        | _ ->
          let num_scalar_ops = tree.num_scalar_ops + 1 in
          {tree with num_scalar_ops=num_scalar_ops}
      end
    | If(_, _, _, _)
    | WhileLoop(_, _, _, _) ->
      helpers.eval_stmt tree stmtNode helpers
end)

let build_work_tree fn = WorkTreeBuilder.eval_fn fn

let rec aux_to_str num_spaces tree =
  match tree.adverb with
  | Some adverb_info ->
    Printf.printf "%s(%d)\n%!"
      (Adverb.to_str adverb_info.adverb)
      tree.num_scalar_ops
    ;
    List.iter (aux_to_str (num_spaces + 2)) tree.nested_adverbs
  | None -> Printf.printf "Root(%d)\n%!" tree.num_scalar_ops

let to_str tree =
  aux_to_str 0 tree
