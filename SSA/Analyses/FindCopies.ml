(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Analysis

module CopyLattice = struct
  type t = Top | Copy of ID.t

  let combine x y = match x,y with
    | Copy id1, Copy id2 -> if id1 = id2 then Copy id1 else Top
    | _ -> Top
end
include  CopyLattice

(* every variable is either a copy of itself or of another variable *)
module CopyEval = MkEvaluator(struct
  type env =  CopyLattice.t ID.Map.t
  type value_info = CopyLattice.t
  type exp_info = CopyLattice.t list

  let iterative = false
  let dir = Forward

  let init fundef =
    List.fold_left
      (fun env id -> ID.Map.add id Top env)
      ID.Map.empty
      fundef.input_ids

  let value _ valNode = match valNode.value with
    | Var id -> Copy id
    | _ -> Top

  let exp env expNode helpers = match expNode.exp with
    | Values vs -> List.map (value env) vs
    | _ -> List.map (fun _ -> Top) expNode.exp_types

  let phi_set env id defVal =
    if ID.Map.mem id env then
      let oldVal = ID.Map.find id env in
      let combined = CopyLattice.combine defVal oldVal in
      (
        if combined <> oldVal then Some (ID.Map.add id combined env)
        else None
      )
    else Some (ID.Map.add id defVal env)

  let phi_merge env id leftVal rightVal =
    phi_set env id (CopyLattice.combine leftVal rightVal)

  let stmt env stmtNode helpers = match stmtNode.stmt with
    | Set(ids, rhs) ->
        let rhsCopies = exp env rhs helpers in
        let changed = ref false in
        let update_copy (env:env) (id:ID.t) (newCopy:CopyLattice.t) =
          if not $ ID.Map.mem id env then
            (changed := true;  ID.Map.add id newCopy env)
          else (
            let oldCopy = ID.Map.find id env in
            let combined = CopyLattice.combine newCopy oldCopy in
            if oldCopy = combined then env
            else (changed := true; ID.Map.add id combined env)
          )
        in
        IFDEF DEBUG THEN
          let nIds = List.length ids in
          let nRhs = List.length rhsCopies in
          if nIds <> nRhs then
            failwith $ Printf.sprintf
              "[FindCopies] Wrong number of IDs on LHS of %s: expected %d, got %d"
              (SSA.stmt_node_to_str stmtNode)
              nRhs
              nIds
            ;
        ENDIF;
        let env' = List.fold_left2 update_copy env ids rhsCopies in
        if !changed then Some env' else None
   | _ -> helpers.eval_stmt env stmtNode
end)

let find_copies f = CopyEval.eval_fn f