(* pp: -parser o pa_macro.cmo *)

open Base
open Imp
open MathEval
open SymbolicShape

let rec eval_dim_as_int (shapeEnv:Shape.t ID.Map.t) (dim:SymbolicShape.dim) =
  match dim with
  | Const i -> i
  | Dim (id, idx) ->
      if ID.Map.mem id shapeEnv then
        begin
          let shape = ID.Map.find id shapeEnv in
          IFDEF DEBUG THEN
            let rank = Shape.rank shape  in
            if rank <= idx then
              failwith $ Printf.sprintf
                "[ShapeEval] Can't evaluate %s since %s's rank is only %d"
                (SymbolicShape.dim_to_str dim)
                (ID.to_str id)
                rank
          ENDIF;
          Shape.get shape idx
        end
      else
        failwith $ Printf.sprintf
          "[ShapeEval] shape not found for %s" (ID.to_str id)
  | Op (op, x, y) ->
      let fn = match op with
        | Add -> (+)
        | Mult ->  ( * )
        | Max -> max
      in
      fn (eval_dim_as_int shapeEnv x) (eval_dim_as_int shapeEnv y)

let rec eval_dim_as_float shapeEnv = function
  | Const i -> float_of_int i
  | Dim (id, idx) ->
      if ID.Map.mem id shapeEnv then
        let shape = ID.Map.find id shapeEnv in
        float_of_int (Shape.get shape idx)
      else
        failwith $ Printf.sprintf
          "[ShapeEval] shape not found for %s" (ID.to_str id)
  | Op (op, x, y) ->
      let fn = match op with
        | Add -> (+.)
        | Mult ->  ( *. )
        | Max -> max
      in fn (eval_dim_as_float shapeEnv x) (eval_dim_as_float shapeEnv y)


let eval_shape (shapeEnv : Shape.t ID.Map.t) (symShape : SymbolicShape.t) =
  let dims = List.map (eval_dim_as_int shapeEnv) symShape in
  Shape.of_list dims

let eval_imp_shape_env (fn : Imp.fn) (inputShapes : Shape.t list) =
  (* shapes of all inputs *)
  let inputEnv : Shape.t ID.Map.t =
    List.fold_left2
      (fun accEnv id shape -> ID.Map.add id shape accEnv)
      ID.Map.empty
      fn.input_ids
      inputShapes
  in
  let aux (id : ID.t) (symShape : SymbolicShape.t) shapeEnv  =
    let shape = eval_shape shapeEnv symShape in
    ID.Map.add id shape shapeEnv
  in
  ID.Map.fold aux fn.shapes inputEnv

let eval_ssa_output_shapes
    (fnTable : FnTable.t)
    (fundef : SSA.fn)
    (inputShapes : Shape.t list) : Shape.t list =
  let symbolicOutputShapes =
    ShapeInference.infer_normalized_output_shapes fnTable fundef
  in
  let shapeEnv = ID.Map.extend ID.Map.empty fundef.SSA.input_ids inputShapes in
  List.map (eval_shape shapeEnv) symbolicOutputShapes

let eval_ssa_shape_env
    (fnTable : FnTable.t)
    (fundef : SSA.fn)
    (inputShapes : Shape.t list) : Shape.t ID.Map.t =
  (* infer symbolic shapes associated with each variable in a function *)
  let symShapes = ShapeInference.infer_normalized_shape_env fnTable fundef in
  (* concrete shapes of the arguments *)
  let initShapeEnv =
    ID.Map.extend ID.Map.empty fundef.SSA.input_ids inputShapes
  in
  (* evaluate symbolic shapes to get concrete shapes *)
  ID.Map.map (eval_shape initShapeEnv) symShapes

let get_call_output_shapes fn (inputs : Shape.t list) =
  IFDEF DEBUG THEN
    let nShapes = List.length inputs in
    let nFormals = List.length fn.input_ids in
    if nShapes <> nFormals then failwith $ Printf.sprintf
      "[ShapeEval] Wrong number of args given, expected %d, got %d"
      nFormals
      nShapes
    ;
  ENDIF;
  let shapeEnv = ID.Map.extend ID.Map.empty fn.input_ids inputs in
  let outputSymShapes : SymbolicShape.t list =
    List.map (fun id -> ID.Map.find id fn.shapes) fn.output_ids
  in
  List.map (eval_shape shapeEnv) outputSymShapes
  (*
  rewrite_shapes replaceMap rawOutputShapes
  *)
