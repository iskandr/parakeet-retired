(* pp: -parser o pa_macro.cmo *)
open Base
open TypedSSA
open SSA_Analysis

let simple_conversion = function
  | Type.ScalarT eltT -> ImpType.ScalarT eltT
  | Type.ArrayT (eltT, rank) -> ImpType.ArrayT (eltT, rank)
  | other -> failwith $ Printf.sprintf
    "[InferImpTypes.simple_conversion] Unexpected type: %s"
    (Type.to_str other)

module type IMP_TYPE_PARAMS = sig val input_imp_types : ImpType.t list end

module ImpTypeAnalysis(P:IMP_TYPE_PARAMS) = struct
  let combine t1 t2 =
    if t1 = t2 then t1
    else failwith $ Printf.sprintf "[InferImpTypes] Can't combine %s and %s"
         (ImpType.to_str t1)
         (ImpType.to_str t2)

  let add_binding tenv id t =
    let t : ImpType.t =
      if ID.Map.mem id tenv then combine t (ID.Map.find id tenv) else t
    in
    ID.Map.add id t tenv

  let add_bindings tenv ids ts = List.fold_left2 add_binding tenv ids ts

  type env = ImpType.t ID.Map.t
  type value_info = ImpType.t
  type exp_info = ImpType.t list

  let dir = Forward
  let iterative = false

  let init fn =
    IFDEF DEBUG THEN
      let nInputs = List.length fn.input_ids in
      let nImpTypes = List.length P.input_imp_types in
      if nInputs <> nImpTypes then
        failwith $ Printf.sprintf
          "[InferImpTypes] Expected %d args for %s, got %d"
          nInputs
          (TypedSSA.PrettyPrinters.fn_id_to_str fn)
          nImpTypes
    ENDIF;
    ID.Map.of_lists fn.input_ids P.input_imp_types

  let value tenv {value; value_type} =
    if Type.is_scalar value_type then simple_conversion value_type
    else match value with
    | TypedSSA.Var id ->
      if not $ ID.Map.mem id tenv then
        failwith $ "ID not found: " ^ ID.to_str id
      else
        ID.Map.find id tenv
    | other ->
      failwith $
        "[ImpInferTypes] invalid value: " ^ (TypedSSA.value_to_str other)

  let infer_array_prim arrayOp args retTypes =
    (* for now just assume every array operator creates a fresh array *)
    List.map simple_conversion retTypes

  let exp tenv {exp; exp_types} helpers : ImpType.t list =
    match exp with
    | Values vs -> List.map (value tenv) vs
    | Cast _
    | PrimApp (Prim.ScalarOp _, _)
    | Adverb _
    | Arr _ -> List.map simple_conversion exp_types
    | PrimApp (Prim.ArrayOp op, args) -> infer_array_prim op args exp_types
    | Call (fnId, args) ->
      failwith "[InferImpTypes] Typed function calls not implemented"
    | PrimApp (p, args) ->
      failwith $ Printf.sprintf
        "[InferImpTypes] Unsupported primitive: %s (with args %s)"
        (Prim.to_str p)
        (TypedSSA.value_nodes_to_str args)
  let phi_set tenv id rhs = Some (add_binding tenv id rhs)
  let phi_merge tenv id _ right = Some (add_binding tenv id right)

  let stmt (tenv:ImpType.t ID.Map.t) stmtNode helpers : env option =
    match stmtNode.stmt with
    | TypedSSA.Set(ids, rhs) ->
      Some (add_bindings tenv ids (exp tenv rhs helpers))
    | _ -> helpers.eval_stmt tenv stmtNode
end


let infer (fn:TypedSSA.fn) (inputTypes:ImpType.t list) : ImpType.t ID.Map.t  =
  let module Param = struct let input_imp_types = inputTypes end in
  let module ImpTypeEval = SSA_Analysis.MkEvaluator(ImpTypeAnalysis(Param)) in
  ImpTypeEval.eval_fn fn
