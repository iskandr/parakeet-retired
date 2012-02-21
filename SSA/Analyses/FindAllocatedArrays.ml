
(* pp: -parser o pa_macro.cmo *)
open Base
open SSA
open SSA_Analysis

(* identify which variables need to be malloc'd and which *)
(* are either scalars or aliases *)
module Eval = SSA_Analysis.MkEvaluator(struct
  type env = ID.Set.t
  type value_info = unit
  type exp_info = bool list

  let dir = Forward
  let iterative = false

  (* initially assume everyone is an alias or scalar until proven otherwise *)
  let init fn = ID.Set.empty

  let value _ _  = ()
  let exp _ {exp; exp_types} _ : bool = match exp with
    | SSA.Typed.Arr _
    | SSA.Typed.PrimApp (Prim.ArrayOp _, _)
    | SSA.Typed.Adverb _ -> [true]
    (* TODO: *)
    (* calls only require allocation if their outputs are arrays *)
    (* OR if their locals are materialized/allocated arrays. We're not*)
    (* checking that second condition here! *)
    | SSA.Typed.Call _ -> List.map Type.is_array exp_types
    | _ -> List.map (fun _ -> false) exp_types

  (* phi nodes can only create array aliases *)
  let phi_set _ _ _ = None
  let phi_merge _ _ _ _ = None

  let stmt (set:ID.Set.t) stmtNode helpers : env option =
    match stmtNode.stmt with
    | SSA.Typed.Set(ids, rhs) ->
      let rhsBools : bool list = exp set rhs helpers in
      Some (List.fold_left2
        (fun set id isAlloc -> if isAlloc then ID.Set.add id set else set)
        ids
        rhsBools)
    | _ -> helpers.eval_stmt tenv stmtNode
end)


let find_allocated_arrays : SSA.Typed.fn -> ID.Set.t = Eval.eval_fn
