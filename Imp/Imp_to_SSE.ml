(* pp: -parser o pa_macro.cmo *)

open Base
open Imp
open ImpType

let vectorize_type width = function
  | ScalarT eltT -> VectorT(eltT, width)
  | _ -> failwith "Unsupported type to vectorize"

let vectorize_shape width = function
  | [] -> [SymbolicShape.Const width]
  | _ -> failwith "Can't vectorize array shape"


let rec vectorize_value width valNode =
  let vecType = vectorize_type width valNode.value_type in
  let newValue, newType = match valNode.value with
  | Const n ->
    VecConst((List.fill n (List.til width))), valNode.value_type
  | Op(argT, op, vs) ->
    Op(vecType, op, vectorize_values width vs), vecType
  | Idx(arr, indices) ->
    assert (List.length indices = 1);
    let idx = List.hd indices in
    VecSlice(arr, idx, width), vecType
  | other -> other, vecType
  in
  {value = newValue; value_type = newType}

and vectorize_values width valNodes =
  List.map (vectorize_value width) valNodes

let rec vectorize_stmt width = function
  | If(cond, tBlock, fBlock) -> If(cond, tBlock, fBlock)
  | While(cond, body) -> While(cond, body)
  | Set(lhs, rhs) ->
    Set(vectorize_value width lhs, vectorize_value width rhs)
  | other ->
    failwith $
      Printf.sprintf "Unvectorizable statement: %s" (Imp.stmt_to_str other)

and vectorize_block width = function
  | [] -> []
  | stmt::rest ->
    let stmt = vectorize_stmt width stmt in
    stmt :: vectorize_block width rest



let vectorize_fn impFn width =
  ID.Map.iter
    (fun a b -> Printf.printf "%s:%s\n%!" (ID.to_str a) (ImpType.to_str b))
    impFn.types
  ;
  let vecTypes = ID.Map.map (vectorize_type width) impFn.types in
  let vecShapes = ID.Map.map (vectorize_shape width) impFn.shapes in
  let vecBlock = vectorize_block width impFn.body in
  {impFn with types = vecTypes; shapes=vecShapes; body = vecBlock}
