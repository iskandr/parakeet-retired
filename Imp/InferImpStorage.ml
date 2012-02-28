(* pp: -parser o pa_macro.cmo *)
open Base
open TypedSSA
open SSA_Analysis

module ImpStorageAnalysis = struct
  let combine s1 s2 =
    if s1 = s2 then s1
    else failwith $
      Printf.sprintf "[InferImpTypes] Can't combine %s and %s"
       (Imp.array_storage_to_str s1)
       (Imp.array_storage_to_str s2)

  let add_binding env id s =
    let s : Imp.storage =
      if ID.Map.mem id env then combine s (ID.Map.find id env) else s
    in
    ID.Map.add id s env

  let add_bindings env ids storages =
    List.fold_left2 add_binding env ids storages

  type env = Imp.storage ID.Map.t
  type value_info = Imp.storage
  type exp_info = Imp.storage list

  let dir = Forward
  let iterative = false

  let init fn =
    List.fold_left
      (fun acc id -> ID.Map.add id Imp.Alias acc)
      ID.Map.empty
      fn.input_ids

  let value tenv {value; value_type} =
    if Type.is_scalar value_type then Imp.Stack
    else match value with
    | TypedSSA.Var id ->
      if not $ ID.Map.mem id tenv then
        failwith $ "ID not found: " ^ ID.to_str id
      else
        ID.Map.find id tenv
    | _ -> assert false

  let exp env {exp; exp_types} helpers : ImpType.t list =
    match exp with
    | Values vs -> List.map (value env) vs
    | Cast _
    | PrimApp (Prim.ScalarOp _, _) -> [Imp.Stack]
    | PrimApp (Prim.ArrayOp op, args)
    | Adverb _
    | Arr _ -> List.map (fun _ -> Imp.HeapAlloc) exp_types

    | Call (fnId, args) ->
      failwith "[InferImpTypes] Typed function calls not implemented"
    | PrimApp (p, args) ->
      failwith $ Printf.sprintf
        "[InferImpTypes] Unsupported primitive: %s (with args %s)"
        (Prim.to_str p)
        (TypedSSA.value_nodes_to_str args)

  let phi_set env id rhs = Some (add_binding env id rhs)
  let phi_merge env id _ right = Some (add_binding env id right)

  let stmt env stmtNode helpers : env option =
    match stmtNode.stmt with
    | TypedSSA.Set(ids, rhs) ->
      Some (add_bindings env ids (exp env rhs helpers))
    | _ -> helpers.eval_stmt tenv stmtNode
end


let infer (fn:TypedSSA.fn) : Imp.storage ID.Map.t  =
  let module ImpStorageEval = SSA_Analysis.MkEvaluator(ImpStorageAnalysis) in
  ImpStorageEval.eval_fn fn
