open Base
open Imp
open Printf

let replace_var (env:Imp.value_node ID.Map.t) (valNode:Imp.value_node) =
  match valNode.value with
    | Var id ->
      if ID.Map.mem id env then
        let replaceNode = ID.Map.find id env in
        if replaceNode.value_type <>  valNode.value_type then 
          failwith $ Printf.sprintf 
            "Type mismatch: old type = %s, new type = %s"
              (ImpType.to_str valNode.value_type)
              (ImpType.to_str replaceNode.value_type)
        ;
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
let rec replace_stmt (env:Imp.value_node ID.Map.t) stmt  =
  let f valNode = replace_value env valNode in
  Imp.recursively_apply_to_stmt ~lhs:f ~rhs:f stmt


and replace_block (env:Imp.value_node ID.Map.t) = function
  | [] -> []
  | stmt::rest ->
    let stmt = replace_stmt env stmt in
    stmt :: (replace_block env rest)
