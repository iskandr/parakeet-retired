(* pp: -parser o pa_macro.cmo *)

open Type
open Base
open TypedSSA
open SSA_Transform

(* expressions without side effects *)
let is_safe_exp expNode = match expNode.exp with
  | PrimApp _ | Arr _  | Values _ -> true
  | _ -> false (* assume function calls unsafe by default *)

(* this is a really weak form of CSE. To start handling control flow*)
(* splits I need to split this into an initial CSE_Analysis which iteratively*)
(* fills an environment, followed by a rewrite *)

module CSE_Rules = struct
  type context = (exp, value) Hashtbl.t
  let init _ = Hashtbl.create 127
  let finalize _ _ = NoChange
  let dir = Forward

  let stmt env stmtNode =  match stmtNode.stmt with
    (* leave simple constants alone *)
    | Set ([id], {exp=Values [{value = Num _}]}) -> NoChange
    | Set ([id], expNode) when is_safe_exp expNode ->
      if Hashtbl.mem env expNode.exp then (
        let rhsVal = Hashtbl.find env expNode.exp in
        let src = expNode.exp_src in
        let expNode' =
          TypedSSA.vals_exp ?src expNode.exp_types [rhsVal]
        in
        Update (TypedSSA.set [id] expNode')
      )
      else (Hashtbl.add env (expNode.exp (Var id)); NoChange)
    | _ -> NoChange
  (* TODO: propagate expressions through phi nodes *)
  let phi   env phiNode = NoChange
  let exp   env envNode = NoChange
  let value env valNode = NoChange
end

module CSE_Rewrite = SSA_Transform.Mk(CSE_Rules)

let cse _ = CSE_Rewrite.transform_fn