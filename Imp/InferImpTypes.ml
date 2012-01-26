(* pp: -parser o pa_macro.cmo *)
open Base
open SSA

type tenv = ImpType.t ID.Map.t

let combine t1 t2 =
  if t1 = t2 then t1
  else failwith $ Printf.sprintf "[InferImpTypes] Can't combine %s and %s"
         (ImpType.to_str t1)
         (ImpType.to_str t2)

let scalar_imp_type t = ImpType.ScalarT (Type.elt_type t)

let infer_value (tenv:tenv) {value; value_type} : ImpType.t =
  if Type.is_scalar value_type then scalar_imp_type value_type
  else match value with
    | SSA.Var id ->
      if not $ ID.Map.mem id tenv then
        failwith $ "ID not found: " ^ ID.to_str id
      else
        ID.Map.find id tenv
    | Num n ->  ImpType.ScalarT (ParNum.type_of n)
    | other -> failwith $ "[ImpInferTypes] invalid value: " ^ (SSA.value_to_str other)

let infer_values tenv vs : ImpType.t list = List.map (infer_value tenv) vs

let infer_exp (tenv:tenv) {exp; exp_types} : ImpType.t list =
  if List.for_all Type.is_scalar exp_types then List.map scalar_imp_type exp_types
  else  match exp with
  | Values vs -> List.map (infer_value tenv) vs
  | App _ -> failwith "[InferImpTypes] Unexpected untyped function application"
  | Arr vs -> failwith "[InferImpTypes] Array literals not yet implemented"
  | Cast (t, v) -> failwith "[InferImpTypes] Unexpected non-scalar cast"
  | Call (fnId, args) -> failwith "[InferImpTypes] Typed function calls not implemented"
  | PrimApp (prim, args) -> failwith "[InferImpTypes] Unexpected non-scalar primitive"
  | Adverb (adverb, closure, adverb_args) -> failwith "[InferImpTypes] adverbs not implemented"

let add_binding tenv id t =
  if ID.Map.mem id tenv then
    ID.Map.add id (combine t (ID.Map.find id tenv)) tenv
  else
    ID.Map.add id t tenv

let add_bindings tenv ids ts =
  List.fold_left2 add_binding tenv ids ts

let infer_phi_node tenv phiNode : tenv =
  let tLeft : ImpType.t = infer_value tenv phiNode.SSA.phi_left in
  let tRight : ImpType.t  = infer_value tenv phiNode.SSA.phi_right in
  let t : ImpType.t  = combine tLeft tRight in
  add_binding tenv phiNode.SSA.phi_id t

let rec infer_phi_nodes tenv = function
  | [] -> tenv
  | p::ps ->
    let tenv' : tenv = infer_phi_node tenv p in
    infer_phi_nodes tenv' ps



let rec infer_stmt (tenv:tenv) stmtNode = match stmtNode.stmt with
  | SSA.Set(ids, rhs) -> add_bindings tenv ids (infer_exp tenv rhs)
  | SSA.If(cond, tBlock, fBlock, phiNodes) ->
      let tenv = infer_block tenv tBlock in
      let tenv = infer_block tenv fBlock in
      infer_phi_nodes tenv phiNodes
  | SSA.WhileLoop(condBlock, condVal, body, header) ->
      let initIds, initValues = SSA_Helpers.collect_phi_values true header in
      let tenv = add_bindings tenv initIds (infer_values tenv initValues) in
      let tenv = infer_block tenv condBlock in
      let tenv = infer_block tenv body in
      let finalIds, finalValues = SSA_Helpers.collect_phi_values false header in
      add_bindings tenv finalIds (infer_values tenv finalValues)
  | other ->
      failwith
        ("[InferImpTypes] Unsupported statement: " ^  (SSA.stmt_node_to_str stmtNode))
and infer_block (tenv:tenv) block =
  Block.fold_forward infer_stmt tenv block

let infer (fn:SSA.fn) (inputTypes:ImpType.t list) : tenv  =
  let tenv = ID.Map.of_lists fn.input_ids inputTypes in
  infer_block tenv fn.body
