open Base
open Imp
open Printf

let apply_id_map_to_val idMap valNode = match valNode.value with
  | Var id -> if ID.Map.mem id idMap then Var (ID.Map.find id idMap) else Var id
  | other -> other

(* FIND/REPLACE identifiers in Imp expression *)
let rec apply_id_map_to_exp idMap expNode =
  let aux_val : Imp.value_node -> Imp.value_node = apply_id_map_to_val idMap in
  let exp' = match expNode.exp with
  | Val v ->  aux_val v
  | Idx (v1, v2) -> Idx (aux_val e1, aux_val v2)
  | Op (op, argT, vs) -> Op (op, argT, List.map aux_val vs)
  | Select (t, v1, v2, v3) -> Select(t, aux_val v1, aux_val v2, aux_val v3)
  | Cast (t1, v) -> Cast (t1, aux_val v)
  | DimSize (n, e) -> DimSize (n, aux_val v)
  | other -> other
  in {expNode with exp = exp'}

(* FIND/REPLACE identifiers in Imp statement *)
let rec apply_id_map_to_stmt idMap stmt =
  let aux_stmt = apply_id_map_to_stmt idMap in
  let aux_exp = apply_id_map_to_exp idMap in
  let aux_val = apply_id_map_to_val idMap in
  match stmt with
  | If (cond,tBlock,fBlock) ->
      If(aux_val cond, List.map aux_stmt tBlock, List.map aux_stmt fBlock)
  | While (cond,body) -> While(aux_exp cond, List.map aux_stmt body)
  | Set (id, rhs) ->
       let rhs' = aux_exp rhs in
       if ID.Map.mem id idMap then  Set(ID.Map.find id idMap, rhs')
       else Set(id, rhs')
  | SetIdx (id, indices,rhs) ->
       let rhs' =  aux_exp rhs in
       let indices' = List.map  aux_val indices in
        if ID.Map.mem id idMap then  SetIdx (ID.Map.find id idMap, indices', rhs')
        else SetIdx(id, indices', rhs')
  | other -> other
and apply_id_map_to_stmts idMap stmts =
  List.map (apply_id_map_to_stmt idMap) stmts

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

