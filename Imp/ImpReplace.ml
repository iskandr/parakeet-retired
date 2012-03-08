open Base
open Imp
open Printf

let replace_var (env:Imp.value_node ID.Map.t) (valNode:Imp.value_node) =
  match valNode.value with
    | Var id ->
      if ID.Map.mem id env then
        let replaceNode = ID.Map.find id env in
        assert (replaceNode.value_type = valNode.value_type);
        replaceNode
      else valNode
    | _ -> valNode

let replace_value (env:Imp.value_node ID.Map.t) (valNode:Imp.value_node) =
  Imp.recursively_apply (replace_var env) valNode

let replace_values
    (env:Imp.value_node ID.Map.t)
    (valNodes:Imp.value_node list) =
  List.map (replace_value env) valNodes

(* FIND/REPLACE identifiers in Imp statement *)
let rec replace_stmt (env:Imp.value_node ID.Map.t) = function
  | If (cond, tBlock, fBlock) ->
    let tBlock : Imp.stmt list = replace_block env tBlock in
    let fBlock : Imp.stmt list = replace_block env fBlock in
    let cond : Imp.value_node = replace_value env cond in
    If(cond, tBlock, fBlock)
  | While (cond, body) ->
    While(replace_value env cond, replace_block env body)
  | Set (lhs, rhs) ->
    Set(replace_value env lhs, replace_value env rhs)
  | other ->
    failwith $ Printf.sprintf
      "Unsupported statement: %s"
      (Imp.stmt_to_str other)
and replace_block (env:Imp.value_node ID.Map.t) = function
  | [] -> []
  | stmt::rest ->
    let stmt = replace_stmt env stmt in
    stmt :: (replace_block env rest)
