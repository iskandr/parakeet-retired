(* pp: -parser o pa_macro.cmo *)

open Base
open Base
open SSA
open SSA_Analysis
open SymbolicShape
(* takes an SSA function and a map of its IDs to their counterparts in some
   future Imp function. Infers an Imp.exp array corresponding to each
   variable's shape
*)

exception ShapeInferenceFailure of string
type env = SymbolicShape.t ID.Map.t
module type PARAMS = sig
  val output_shapes : FnId.t -> SymbolicShape.t list -> SymbolicShape.t list
end

module ShapeAnalysis (P: PARAMS) =  struct
    type value_info = SymbolicShape.t
    type exp_info = value_info list
    type env = value_info ID.Map.t

    let dir = Forward

     let clone_env env = env

    (* should analysis be repeated until environment stops changing? *)
    let iterative = true

    (* TODO: add memoization on function ID here *)
    let init fundef =
      List.fold_left
        (fun accEnv id ->
          let rank = Type.rank (ID.Map.find id fundef.SSA.tenv) in
          let shape = SymbolicShape.all_dims id rank in
          ID.Map.add id shape accEnv
        )
        ID.Map.empty
        fundef.input_ids

    let value env valNode = match valNode.value with
      | SSA.Var id -> SymbolicShape.all_dims id (Type.rank valNode.SSA.value_type)
      | _ -> [] (* empty list indicates a scalar *)

    let phi_set env id shape =
      if ID.Map.mem id env then (
        let oldShape = ID.Map.find id env in
        if shape <> oldShape then failwith "Shape error"
        else None
      )
      else Some (ID.Map.add id shape env)

    let phi_merge env id leftShape rightShape =
      if leftShape <> rightShape then failwith "Shape error";
      phi_set env id leftShape

    let infer_adverb
          (adverb:Prim.adverb)
          (fnId:FnId.t)
          (closureArgs:SymbolicShape.t list)
          (axes: int list)
          (init:SymbolicShape.t list option)
          (args:SymbolicShape.t list) =
      match adverb with
        | Prim.Map ->
          assert (init = None);
          let maxOuterShape, maxRank = SymbolicShape.argmax_rank args in
          assert (maxRank >= List.length axes);
          let eltShapes = SymbolicShape.peel_shape_list ~axes args in
          (* shapes that go into the function on each iteration *)
          let inputShapes = closureArgs @ eltShapes in
          let callResultShapes = P.output_shapes fnId inputShapes in
          let outerDims =
            SymbolicShape.get_dims maxOuterShape axes
          in
          List.map (fun shape -> outerDims @ shape) callResultShapes

        | Prim.Reduce -> assert false
        | _ ->
          failwith $ Printf.sprintf
            "[ShapeInference] Unsupported adverb %s"
            (Prim.adverb_to_str adverb)

    let infer_array_op
          (op:Prim.array_op)
          (args:SymbolicShape.t list) : SymbolicShape.t =
      match op, args with
      | Prim.Index, (arrayShape::indexShapes) ->
        let nIndices = List.length indexShapes in
        if List.for_all SymbolicShape.is_scalar indexShapes then (
        (* for now assume slicing can only happen along the
           outermost dimensions and only by scalar indices
         *)
          IFDEF DEBUG THEN
            let rank =  SymbolicShape.rank arrayShape in
            if rank < nIndices then
              failwith $ Printf.sprintf
                "[ShapeInference] %d indices can't index into rank %d array"
                rank
                nIndices
          ENDIF;
          List.drop nIndices arrayShape
        )
        (* for now we're also allowing index vectors, though this really
           ought to become a map
         *)
        else (
          IFDEF DEBUG THEN
            if nIndices <> 1 then
              failwith
                "[ShapeInference] Indexing by multiple arrays not supported"
          ENDIF;
          let idxShape = List.hd indexShapes  in
          SymbolicShape.concat idxShape (SymbolicShape.peel arrayShape)
        )
      | Prim.Where, _ ->
        let msg =
          "Shape of 'where' operator cannot be statically determined"
        in
        raise (ShapeInferenceFailure msg)
      | Prim.DimSize, [_; _]
      | Prim.Find, [_; _] -> SymbolicShape.scalar
      | _ ->
        failwith "Unsupported array operator %s with args %s"

    let infer_primapp
          (op:Prim.prim)
          (args : SymbolicShape.t list) : SymbolicShape.t list =
      match op, args with
      | Prim.ArrayOp arrayOp, _ -> [infer_array_op arrayOp args]
      | Prim.ScalarOp _, args  when List.for_all SymbolicShape.is_scalar args ->
        [SymbolicShape.scalar]
      | _ ->
        failwith $ Printf.sprintf
        "[ShapeInference] Unsupported primitive %s with args %s"
        (Prim.to_str op)
        (SymbolicShape.shapes_to_str args)



    let exp env expNode helpers =
      let shapes_of_values valNodes = List.map (value env) valNodes in
      match expNode.exp with
      | SSA.Call(fnId, args) ->
          P.output_shapes fnId (shapes_of_values args)
      | SSA.PrimApp (op, args) ->
        let argShapes = shapes_of_values args in
        infer_primapp op argShapes
      | SSA.Arr elts ->
        let eltShapes = List.map (value env) elts in
        (* TODO: check that elt shapes actually match each other *)
        let n = List.length eltShapes in
        [SymbolicShape.Const n :: (List.hd eltShapes)]
      | SSA.Cast (t, v) ->  [value env v]
      | SSA.Values vs -> shapes_of_values vs
      | SSA.Adverb (adverb, closure, {axes; init; args}) ->
        let closureArgShapes = shapes_of_values closure.SSA.closure_args in
        let argShapes = shapes_of_values args in
        let initShapes = Option.map shapes_of_values init in
        let fnId = closure.SSA.closure_fn in
        infer_adverb adverb fnId closureArgShapes axes initShapes argShapes
      | _ ->
          let expStr = SSA.exp_to_str expNode in
          failwith (Printf.sprintf "[shape_infer] not implemented: %s\n" expStr)

      let stmt env stmtNode helpers = match stmtNode.stmt with
      | Set(ids, rhs) ->
          let newShapes = exp env rhs helpers in
          IFDEF DEBUG THEN
            if List.length ids <> List.length newShapes then
              failwith ("Shape inference error in stmt '" ^
                        (SSA.stmt_node_to_str stmtNode) ^
                        "': number of IDs must match number of rhs shapes");
          ENDIF;
          let prevDefined = List.for_all (fun id ->ID.Map.mem id env) ids in
          let changed =
            not prevDefined ||
            let oldShapes = List.map (fun id -> ID.Map.find id env) ids in
            List.exists2 (<>) oldShapes newShapes
          in
          if not changed then None
          else
          let env' =
            List.fold_left2
              (fun env id shape -> ID.Map.add id shape env)
              env ids newShapes
          in
          Some env'
      | _ -> helpers.eval_stmt env stmtNode

end


(* given a dim expression (one elt of a shape) which may reference
   intermediate variables, "normalize" it to only refer to input variables
*)

let rec normalize_dim
          (inputSet: ID.Set.t)
          (rawShapeEnv: SymbolicShape.t ID.Map.t)
          (normalizedEnv: SymbolicShape.t ID.Map.t)  = function
  | Dim (id, idx) when not (ID.Set.mem id inputSet) ->
    if ID.Map.mem id normalizedEnv then
      let shape = ID.Map.find id normalizedEnv in
      SymbolicShape.get_dim shape idx, normalizedEnv
    else
      let rawShape = ID.Map.find id rawShapeEnv in
      let normalizedShape, normalizedEnv' =
        normalize_shape inputSet rawShapeEnv normalizedEnv rawShape
      in
      let normalizedEnv'' = ID.Map.add id normalizedShape normalizedEnv' in
      SymbolicShape.get_dim normalizedShape idx, normalizedEnv''
  | Op (op, x, y) ->
    let x', normalizedEnv' = normalize_dim inputSet rawShapeEnv normalizedEnv x in
    let y', normalizedEnv'' = normalize_dim inputSet rawShapeEnv normalizedEnv' y in
    let resultDim = SymbolicShape.simplify_op op x' y' in
    resultDim, normalizedEnv''
  | other -> other, normalizedEnv
and normalize_shape inputSet rawShapeEnv normalizedEnv shape
    : SymbolicShape.t * SymbolicShape.t ID.Map.t  =
  let foldFn (revDims, normalizedEnv) currDim =
    let currDim', normalizedEnv' =
      normalize_dim inputSet rawShapeEnv normalizedEnv currDim
    in
    currDim' :: revDims, normalizedEnv'
  in

  let revShape, env' = List.fold_left foldFn ([], normalizedEnv) shape in
  List.rev revShape, env'

let rec normalize_shape_list inputSet rawShapeEnv normalizedEnv = function
  | [] -> [], normalizedEnv
  | shape::rest ->
      let rest', normalizedEnv' =
        normalize_shape_list inputSet rawShapeEnv normalizedEnv rest
      in
      let shape', normalizedEnv'' =
        normalize_shape inputSet rawShapeEnv normalizedEnv shape
      in
      shape'::rest', normalizedEnv''



(* cache the normalized output shape expressions of each function. "normalized"
   means the expressions refer to input IDs, which need to be replaced by some
   input expression later
*)
let normalizedOutputShapeCache : (FnId.t, SymbolicShape.t list) Hashtbl.t =
  Hashtbl.create 127

let shapeEnvCache : (FnId.t, SymbolicShape.env) Hashtbl.t = Hashtbl.create 127
(* like the shape env cache, except the shape expressions have all been
   normalize to refer only to input variables and not intermediates
*)
let normalizedShapeEnvCache : (FnId.t, SymbolicShape.env) Hashtbl.t =
  Hashtbl.create 127


let rec infer_shape_env (fnTable:FnTable.t) (fundef : SSA.fn) =
  let fnId = fundef.SSA.fn_id in
  (*
  IFDEF DEBUG THEN
    Printf.printf
      "[ShapeInference::infer_shape_env] Looking up shape env for %s\n"
      (FnId.to_str fnId);
  ENDIF;
  *)
  match Hashtbl.find_option shapeEnvCache fnId with
    | Some shapeEnv -> shapeEnv
    | None  ->
      begin
        let module Params : PARAMS = struct
          let output_shapes fnId argShapes =
            let fundef = FnTable.find fnId fnTable in
            infer_call_result_shapes fnTable fundef argShapes
        end
        in
        let module ShapeEval = SSA_Analysis.MkEvaluator(ShapeAnalysis(Params)) in
        let shapeEnv = ShapeEval.eval_fn fundef in
        Hashtbl.add shapeEnvCache fnId shapeEnv;
        shapeEnv
      end

and infer_normalized_shape_env (fnTable : FnTable.t) (fundef : SSA.fn) =
  let fnId = fundef.SSA.fn_id in
  match Hashtbl.find_option normalizedShapeEnvCache fnId with
    | Some normalizedEnv -> normalizedEnv
    | None ->
      begin
        let rawShapeEnv = infer_shape_env fnTable fundef in
        let inputIdSet = ID.Set.of_list fundef.SSA.input_ids in
        let normalizer id shape normalizedEnv =
          (* if already normalized, don't do it again *)
          if ID.Map.mem id normalizedEnv then normalizedEnv
          else
            let shape', normalizedEnv' =
              normalize_shape inputIdSet rawShapeEnv normalizedEnv shape
            in
            ID.Map.add id shape' normalizedEnv'
        in
        let normalizedEnv = ID.Map.fold normalizer rawShapeEnv ID.Map.empty in
        Hashtbl.add normalizedShapeEnvCache fnId normalizedEnv;
        normalizedEnv
      end

and infer_normalized_output_shapes (fnTable : FnTable.t) (fundef : SSA.fn) =
  let fnId = fundef.SSA.fn_id in
  try Hashtbl.find normalizedOutputShapeCache fnId
  with _ -> begin
    let shapeEnv = infer_shape_env fnTable fundef in
    let inputSet = ID.Set.of_list fundef.input_ids in
    let rawShapes =
      List.map (fun id -> ID.Map.find id shapeEnv) fundef.output_ids
    in
    let normalizedShapes, _ =
      normalize_shape_list inputSet shapeEnv ID.Map.empty rawShapes
    in
    Hashtbl.add normalizedOutputShapeCache fnId normalizedShapes;
    normalizedShapes
 end

and infer_call_result_shapes fnTable fundef argShapes =
  let normalizedOutputShapes = infer_normalized_output_shapes fnTable fundef in
  (* once the shape expressions only refer to input IDs,
     remap those input IDs argument expressions
   *)
  let argEnv : SymbolicShape.t ID.Map.t =
    List.fold_left2
      (fun env id argShape -> ID.Map.add id argShape env)
      ID.Map.empty
      fundef.SSA.input_ids
      argShapes
  in
  let resultShapes =
    List.map (SymbolicShape.rewrite_shape argEnv) normalizedOutputShapes
  in
  resultShapes


