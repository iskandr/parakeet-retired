(* pp: -parser o pa_macro.cmo *)

open Base
open Imp
open ImpCodegen
open ImpHelpers
open ImpType

(* are these necessary? or should we just register each SSA variable with its *)
(* existing name as an imp variable and then implicitly keep this information *)
(* on the codegen object? *)

type loop_descr = {
  loop_var : value_node;
  loop_start : value_node;
  loop_test_val : value_node;
  loop_test_cmp : Prim.scalar_op;
  loop_incr : value_node;
  loop_incr_op : Prim.scalar_op;
}

let rec build_loop_nests
          (codegen:ImpCodegen.codegen)
          (descrs : loop_descr list)
          (body:Imp.block) =
  match descrs with
    | [] -> body
    | d::ds ->
        let nested = build_loop_nests codegen ds body in
        let testEltT = ImpType.elt_type d.loop_var.value_type in
        let test = {
          exp = Imp.Op(testEltT, d.loop_test_cmp,
                       [d.loop_var; d.loop_test_val]);
          exp_type = ImpType.bool_t
        }
        in
        let next = {
           exp = Imp.Op(testEltT, d.loop_incr_op, [d.loop_var; d.loop_incr]);
           exp_type = d.loop_var.value_type;
        }
        in
        let update = set d.loop_var next in
        [
          set_val d.loop_var d.loop_start;
          Imp.While (test , nested @ [update])
        ]

let mk_simple_loop_descr
        (codegen:ImpCodegen.codegen)
        ?(down=false)
        ?(start=ImpHelpers.zero)
        ~(stop:Imp.value_node) =
  {
    loop_var = codegen#fresh_local int32_t;
    loop_start = start;
    loop_test_val = stop;
    loop_test_cmp = (if down then Prim.Lt else Prim.Gt);
    loop_incr = (if down then ImpHelpers.int (-1) else ImpHelpers.one);
    loop_incr_op = Prim.Add;
  }

let translate_value (codegen:ImpCodegen.codegen) valNode : Imp.value_node =
  match valNode.SSA.value with
  | SSA.Var id -> codegen#var id
  | SSA.Num n -> {value = Imp.Const n;
                  value_type = ImpType.ScalarT (ParNum.type_of n)}
  | other -> failwith $
      "[ssa->imp] unrecognized value: " ^ (SSA.value_to_str other)

let rec translate_values codegen valNodes : Imp.value_node list =
  List.map (translate_value codegen) valNodes

let translate_array_op codegen (op:Prim.array_op) (args:Imp.value_node list) =
  match op, args with
    | Prim.Index, array::indices ->
      let arrayT = array.Imp.value_type  in
      IFDEF DEBUG THEN
        let nIndices = List.length indices in
        let rank = ImpType.rank arrayT in
        if rank <> nIndices then
          failwith $ Printf.sprintf
            "[SSA_to_Imp] Mismatch between # dims (%d) and # of indices (%d)"
            rank
            nIndices
      ENDIF;
      let resultT =  ImpType.elt_type arrayT in
      { exp = Imp.Idx(array, indices); exp_type = ImpType.ScalarT resultT }
    | other, _ -> failwith $
      Printf.sprintf
        "[SSA_to_Imp] Unsupported array op: %s"
        (Prim.array_op_to_str other)


let translate_array_literal
      (codegen:ImpCodegen.codegen)
      (lhsId:ID.t)
      (elts:SSA.value_node list)  =
  let ssaTypes = List.map (fun {SSA.value_type} -> value_type) elts in
  if not (List.for_all Type.is_scalar ssaTypes) then
    failwith "[SSA_to_Imp] Nested arrays not yet implemented"
  else
    let n = List.length elts in
    let firstType = List.hd ssaTypes in
    let impType = ImpType.ArrayT (Type.elt_type firstType, 1) in
    let shape = SymbolicShape.Const n in
    let lhs = codegen#var lhsId in
    let impElts : Imp.value_node list = translate_values codegen elts in
    let assign_elt idx rhs =
      ImpHelpers.setidx lhs [ImpHelpers.int idx] rhs
    in
    List.mapi assign_elt impElts

let translate_exp (codegen:ImpCodegen.codegen) expNode : Imp.exp_node  =
  match expNode.SSA.exp with
    | SSA.Values [v] -> ImpHelpers.exp_of_val (translate_value codegen v)
    | SSA.Values _ -> failwith "multiple value expressions not supported"
    | SSA.PrimApp (Prim.ScalarOp op, args) ->
      let args' = translate_values codegen args in
      let opType, returnType =
        if Prim.is_comparison op then
          let firstArg = List.hd args' in
          ImpType.elt_type firstArg.value_type, Type.BoolT
        else
          let retT = Type.elt_type (List.hd expNode.SSA.exp_types) in
          retT, retT
      in
      { exp = Op( opType, op, args'); exp_type = ImpType.ScalarT returnType }
    | SSA.PrimApp (Prim.ArrayOp op, args) ->
      let impArgs = translate_values codegen args in
      translate_array_op codegen op impArgs
    | SSA.Cast(t, src) ->
      (* cast only operates on scalar types! *)
      let eltT = Type.elt_type t in
      let impT = ImpType.ScalarT eltT in
      { exp = Imp.Cast(impT, translate_value codegen src); exp_type = impT }
    | SSA.Arr _ -> failwith "[SSA_to_Imp] Unexpected array expression"
    | _ -> failwith $ "[ssa->imp] unrecognized exp: " ^ (SSA.exp_to_str expNode)

let mk_set_val codegen (id:ID.t) (v:SSA.value_node) =
  let impVar = codegen#var id in
  let impVal = translate_value codegen v in
  set impVar (ImpHelpers.exp_of_val impVal)

let mk_set_exp codegen (id:ID.t) (e:SSA.exp_node) =
  let impExp : Imp.exp_node = translate_exp codegen e in
  let impVar : Imp.value_node = codegen#var id in
  set impVar impExp

let translate_true_phi_node codegen phiNode =
  let rhs = translate_value codegen phiNode.SSA.phi_left in
  Imp.Set(phiNode.SSA.phi_id, ImpHelpers.exp_of_val rhs)

let translate_false_phi_node codegen phiNode =
  let rhs = translate_value codegen phiNode.SSA.phi_right in
  Imp.Set(phiNode.SSA.phi_id, ImpHelpers.exp_of_val rhs)

let rec translate_block (codegen : ImpCodegen.codegen) block : Imp.stmt list =
  Block.fold_forward (fun acc stmt -> acc @ (translate_stmt codegen stmt))
                     [] block
and translate_stmt (codegen : ImpCodegen.codegen) stmtNode : Imp.stmt list  =
  match stmtNode.SSA.stmt with
    (* array literals get treated differently from other expressions since*)
    (* they require a block of code rather than simply translating from *)
    (* SSA.exp_node to Imp.exp_node *)
    | SSA.Set([id], {SSA.exp = SSA.Arr elts}) ->
      translate_array_literal codegen id elts
    (* all assignments other than those with an array literal RHS *)
	  | SSA.Set([id], rhs) -> [mk_set_exp codegen id rhs]
    | SSA.Set(ids, {SSA.exp = SSA.Values vs}) ->
      List.map2 (mk_set_val codegen) ids vs
	  | SSA.Set _ -> failwith "multiple assignment not supported"
    | SSA.SetIdx(lhs, indices, rhs) ->
      let indices : Imp.value_node list = translate_values codegen indices in
      let lhs : Imp.value_node = translate_value codegen lhs in
      let rhs : Imp.value_node = translate_value codegen rhs in
      [Imp.SetIdx(lhs, indices, rhs)]

    | SSA.If(cond, tBlock, fBlock, phiNodes) ->
	    let cond' : Imp.value_node = translate_value codegen cond in
	    let tBlock' : Imp.block = translate_block codegen tBlock in
	    let fBlock' : Imp.block = translate_block codegen fBlock in
	    let trueMerge = List.map (translate_true_phi_node codegen) phiNodes in
	    let falseMerge = List.map (translate_false_phi_node codegen) phiNodes in
	    let tBlock' = tBlock' @ trueMerge in
	    let fBlock' = fBlock' @ falseMerge in
	    [Imp.If(cond', tBlock', fBlock')]
    | SSA.WhileLoop(condBlock, condVal, body, phiNodes) ->
      let inits : Imp.block =
        List.map (translate_true_phi_node codegen) phiNodes
      in
      let condBlock : Imp.block  = translate_block codegen condBlock in
      let condVal : Imp.value_node = translate_value codegen condVal in
      let body : Imp.block = translate_block codegen body in
      let finals = List.map (translate_false_phi_node codegen) phiNodes in
      let fullBody = body @ finals @ condBlock in
      inits @ condBlock @ [Imp.While(ImpHelpers.exp_of_val condVal, fullBody)]
  | _ ->
    failwith $ Printf.sprintf "[Imp_to_SSA] Not yet implemented: %s"
      (SSA.stmt_node_to_str stmtNode)

let translate  (ssaFn:SSA.fn) (impInputTypes:ImpType.t list) : Imp.fn =
  let codegen = new ImpCodegen.fn_codegen in
  let impTyEnv = InferImpTypes.infer ssaFn impInputTypes in
  let shapeEnv : SymbolicShape.env  =
    ShapeInference.infer_normalized_shape_env
        (FnManager.get_typed_function_table ()) ssaFn
  in
  let declare_var (id, impType) : unit =
    if List.mem id ssaFn.SSA.input_ids then
      codegen#declare_input id impType
    else (
      (* inputs all have the trivial shape
          SHAPE(x) = [dim(x,0), dim(x,1), etc...]
         but outputs and locals actually have non-trivial
         shapes which need to be declared
      *)
      let symShape = ID.Map.find id shapeEnv in
      if List.mem id ssaFn.SSA.output_ids then
        codegen#declare_output id ~shape:symShape impType
      else
        codegen#declare id ~shape:symShape impType
    )
  in
  List.iter declare_var (ID.Map.to_list impTyEnv);
  let body = translate_block (codegen :> ImpCodegen.codegen) ssaFn.SSA.body in
  codegen#finalize_fn body
