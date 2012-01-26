open Base
open Imp
open ImpType
open ImpHelpers
open ImpCodegen



(* are these necessary? or should we just register each SSA variable with its existing name as an imp variable*)
(* and then implicitly keep this information on the codegen object? *)

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
          exp = Imp.Op(testEltT, d.loop_test_cmp, [d.loop_var; d.loop_test_val]);
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
  { loop_var = codegen#fresh_local int32_t;
    loop_start = start;
    loop_test_val = stop;
    loop_test_cmp = (if down then Prim.Lt else Prim.Gt);
    loop_incr = (if down then ImpHelpers.int (-1) else ImpHelpers.one);
    loop_incr_op = Prim.Add;
  }


let translate_value (codegen:ImpCodegen.codegen) valNode : Imp.value_node =
  match valNode.SSA.value with
  | SSA.Var id -> codegen#var id
  | SSA.Num n -> {value = Imp.Const n; value_type = ImpType.ScalarT (ParNum.type_of n)}
  | other -> failwith $ "[ssa->imp] unrecognized value: " ^ (SSA.value_to_str other)

let translate_exp (codegen:ImpCodegen.codegen) expNode : Imp.exp_node  =
  match expNode.SSA.exp with
  | SSA.Values [v] -> ImpHelpers.exp_of_val (translate_value codegen v)
  | SSA.Values _ -> failwith "multiple value expressions not supported"
  | SSA.PrimApp (Prim.ScalarOp op, args) ->
    let args' = List.map (translate_value codegen) args in
    let eltT = Type.elt_type (List.hd expNode.SSA.exp_types) in
    { exp = Op( eltT, op, args'); exp_type = ImpType.ScalarT eltT }
  | SSA.PrimApp _ -> failwith "unsupported primitive"
  | SSA.Cast(t, src) ->
      (* cast only operates on scalar types! *)
      let eltT = Type.elt_type t in
      let impT = ImpType.ScalarT eltT in
      {
        exp = Imp.Cast(impT, translate_value codegen src);
        exp_type = impT
      }
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
  Block.fold_forward (fun acc stmt -> acc @ (translate_stmt codegen stmt)) [] block
and translate_stmt (codegen : ImpCodegen.codegen) stmtNode : Imp.stmt list  =
  match stmtNode.SSA.stmt with
	| SSA.Set([id], rhs) -> [mk_set_exp codegen id rhs]
  | SSA.Set(ids, {SSA.exp= SSA.Values vs} ) ->
    List.map2 (mk_set_val codegen) ids vs
	| SSA.Set _ -> failwith "multiple assignment not supported"
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
    ShapeInference.infer_normalized_shape_env (FnManager.get_typed_function_table ()) ssaFn
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
