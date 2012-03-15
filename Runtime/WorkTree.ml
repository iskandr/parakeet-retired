(* pp: -parser o pa_macro.cmo *)

open Base
open SSA_Analysis
open TypedSSA

type t = {
  adverb : adverb_info option;
  nested_adverbs : t list;
  num_scalar_ops : int
}

module WorkTreeBuilder = SSA_Analysis.MkEvaluator(struct
  type env = t
  type value_info = unit
  type exp_info = unit

  let dir = Forward
  let iterative = false

  let init fn = {adverb=None; nested_adverbs=[]; num_scalar_ops=0}

  let value _ _ = ()
  let exp _ _ _ = ()

  let phi_set _ _ _ = None
  let phi_merge set id _ _ = None

  let stmt set stmtNode helpers = None
end)

let build_work_tree fn = WorkTreeBuilder.eval_fn fn
