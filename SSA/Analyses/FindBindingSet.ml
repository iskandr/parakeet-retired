open Base
open TypedSSA
open SSA_Analysis

module BindingSetEval = SSA_Analysis.MkEvaluator(struct
  type env = ID.t MutableSet.t
  type value_info = unit
  type exp_info = unit

  let dir = Forward
  let iterative = false

  let init fundef =
    let set = MutableSet.create 17 in
    List.iter (MutableSet.add set) fundef.input_ids;
    set

  let value _ _ = ()
  let exp _ _ _ = ()


  let phi_set _ _ _ = None
  let phi_merge set id _ _ = MutableSet.add set id; None

  let stmt set stmtNode helpers = match stmtNode.stmt with
    | Set(ids, _) -> List.iter (MutableSet.add set) ids; None
    | _ -> helpers.eval_stmt set stmtNode
end)

let fn_bindings fn = BindingSetEval.eval_fn fn
let block_bindings block =
  let freshSet = MutableSet.create 17 in
  BindingSetEval.eval_block freshSet block