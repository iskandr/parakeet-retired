open Base
open Imp
open Printf



let replace_value (env:Imp.value_node ID.Map.t) (valNode:Imp.value_node) =
  match valNode.value with
  | Var id when ID.Map.mem id env ->
    let replaceNode = ID.Map.find id env in
    assert (replaceNode.value_type = valNode.value_type);
    replaceNode
  | _ -> valNode

let replace_values (env:Imp.value_node ID.Map.t) valNodes =
  List.map (replace_value env) valNodes

let rec replace_exp (env:Imp.value_node ID.Map.t) (expNode:Imp.exp_node) =
  let exp' = match expNode.exp with
  | Val v ->  Val (replace_value env v)
  | Idx (arr, indices) ->
    Idx (replace_value env arr, replace_values env indices)
  | Op (op, argT, vs) -> Op (op, argT, replace_values env vs)
  | Select (t, x, y, z) ->
    Select(t, replace_value env x, replace_value env y, replace_value env z)
  | Cast (t1, v) -> Cast (t1, replace_value env v)
  | DimSize (n, e) -> DimSize (n, replace_value env e)
  | other -> other
  in {expNode with exp = exp'}

(* FIND/REPLACE identifiers in Imp statement *)
let rec replace_stmt (env:Imp.value_node ID.Map.t) = function
  | If (cond, tBlock, fBlock) ->
    let tBlock : Imp.stmt list = replace_block env tBlock in
    let fBlock : Imp.stmt list  = replace_block env fBlock in
    let cond : Imp.value_node = replace_value env cond in
    If(cond, tBlock, fBlock)
  | While (cond, body) ->
    While(replace_exp env cond, replace_block env body)
  | Set (id, rhs) ->
    let rhs = replace_exp env rhs in
    begin match ID.Map.find_option id env with
      | None -> Set(id, rhs)
      | Some {value=Imp.Var id} -> Set(id, rhs)
      | Some other -> failwith $ Printf.sprintf
        "Unexpected lhs replacement %s -> %s"
        (ID.to_str id)
        (Imp.val_node_to_str other)
    end
  | SetIdx ({value=Var id} as lhs, indices,rhs) ->
    let rhs = replace_value env rhs in
    let indices = replace_values env indices in
    begin match ID.Map.find_option id env with
      | None -> SetIdx(lhs, indices, rhs)
      | Some ({value=Var id} as newLhs) ->
        SetIdx(newLhs, indices, rhs)
    end
  | other -> other
and replace_block (env:Imp.value_node ID.Map.t) = function
  | [] -> []
  | stmt::rest ->
    let stmt = replace_stmt env stmt in
    stmt :: (replace_block env rest)
(*
let fresh_fn fn =
  let inputIds' = ID.map_fresh fn.input_ids in
  let outputIds' = ID.map_fresh fn.output_ids in
  let localIds' = ID.map_fresh fn.local_ids in
  let oldIds = fn.input_ids @ fn.local_ids @ fn.output_ids in
  let newIds = inputIds' @ localIds' @ outputIds' in
  let idEnv = ID.Map.extend idEnv oldIds newIds in
  let body' = apply_id_map_to_stmts idEnv fn.body in
  let apply_env oldId = ID.Map.find oldId idEnv in
  { input_ids = inputIds';
    output_ids = outputIds';
    local_ids = localIds';
    body = body';
    types = ID.Map.map apply_env fn.types;
    shapes = ID.Map.map apply_env fn.shapes;
    storage = ID.Map.map apply_env fn.storage;
  }

*)