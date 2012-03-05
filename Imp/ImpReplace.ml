open Base
open Imp
open Printf

let rec replace_value (env:Imp.value_node ID.Map.t) (valNode:Imp.value_node) =
  let value' = match valNode.value with
  | Var id ->
    if ID.Map.mem id env then
      let replaceNode = ID.Map.find id env in
      assert (replaceNode.value_type = valNode.value_type);
      replaceNode.value
    else Var id
  | Idx (arr, indices) ->
    let arr = replace_value env arr in
    let indices = replace_values env indices in
    begin match arr with
      | {value=Var id} when ID.Map.mem id env ->
        (match ID.Map.find id env with
          | {value=Var id'} -> Var id'
          | {value=Idx(arr', indices')} -> Idx(arr', indices' @ indices)
          | _ -> failwith "Unexpected new LHS replacement"
        )
      | {value} -> value
    end
  | Op (argT, op, vs) -> Op (argT, op, replace_values env vs)
  | Select (t, x, y, z) ->
    Select(t, replace_value env x, replace_value env y, replace_value env z)
  | Cast (t1, v) -> Cast (t1, replace_value env v)
  | DimSize (n, e) -> DimSize (replace_value env n, replace_value env e)
  | other -> other
  in {valNode with value = value'}

and replace_values
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
  | Set (id, rhs) ->
    let rhs = replace_value env rhs in
    begin match ID.Map.find_option id env with
      | None -> Set(id, rhs)
      | Some {value=Imp.Var id} -> Set(id, rhs)
      | Some {value=Imp.Idx(array, indices)} -> SetIdx(array, indices, rhs)
      | Some other -> failwith $ Printf.sprintf
        "Unexpected lhs replacement %s -> %s"
        (ID.to_str id)
        (Imp.value_node_to_str other)
    end
  | SetIdx ({value=Var id} as lhs, indices, rhs) ->
    let rhs = replace_value env rhs in
    let indices = replace_values env indices in
    begin match ID.Map.find_option id env with
      | None -> SetIdx(lhs, indices, rhs)
      | Some ({value=Var id} as newLhs) ->
        SetIdx(newLhs, indices, rhs)
      | Some {value=Idx(lhs', indices')} ->
        SetIdx(lhs', indices' @ indices, rhs)
    end
  | other ->
    failwith $ Printf.sprintf
      "Unsupported statement: %s"
      (Imp.stmt_to_str other)
and replace_block (env:Imp.value_node ID.Map.t) = function
  | [] -> []
  | stmt::rest ->
    let stmt = replace_stmt env stmt in
    stmt :: (replace_block env rest)
