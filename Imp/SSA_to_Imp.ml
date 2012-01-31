(* pp: -parser o pa_macro.cmo *)

open Base
open Imp
open ImpCodegen
open ImpHelpers
open ImpType
open ImpReplace

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
          value = Imp.Op(testEltT, d.loop_test_cmp,
                       [d.loop_var; d.loop_test_val]);
          value_type = ImpType.bool_t
        }
        in
        let next = {
           value = Imp.Op(testEltT, d.loop_incr_op, [d.loop_var; d.loop_incr]);
           value_type = d.loop_var.value_type;
        }
        in
        let update = set d.loop_var next in
        [
          set d.loop_var d.loop_start;
          Imp.While (test , nested @ [update])
        ]

let mk_simple_loop_descriptor
        (codegen:ImpCodegen.codegen)
        ?(down=false)
        ?(start=ImpHelpers.zero)
        (stop:Imp.value_node) =
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
      { value = Imp.Idx(array, indices);
        value_type = ImpType.ScalarT resultT
      }
    | other, _ -> failwith $
      Printf.sprintf
        "[SSA_to_Imp] Unsupported array op: %s"
        (Prim.array_op_to_str other)

(* given a list of imp values, return the array of maximum rank *)
let rec argmax_array_rank = function
  | [] -> assert false
  | [x] -> x
  | x::xs ->
    let restResult = argmax_array_rank xs in
    if ImpType.rank x.value_type > ImpType.rank restResult.value_type then x
    else restResult


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

(* given an array, map a list of its axes into *)
(* a list of statements and a list of dimsize values *)
let rec size_of_axes
          (codegen:ImpCodegen.codegen)
          (array:Imp.value_node)
          (axes:int list) : Imp.block * Imp.value_node list =
  match axes with
  | [] -> [], []
  | axis::rest ->
    let size : Imp.value_node = ImpHelpers.dim array (ImpHelpers.int axis) in
    let temp = codegen#fresh_local ImpType.int32_t in
    let stmtNode = ImpHelpers.set temp size in
    let restBlock, restVals = size_of_axes codegen array rest in
    stmtNode :: restBlock, temp :: restVals

(* given an array and a list of axes, create a list of loop descriptors*)
(* which we can turn into nested loops over the array *)
let rec axes_to_loop_descriptors
          (codegen:ImpCodegen.codegen)
          (array : Imp.value_node)
          (axes : int list) : Imp.block * loop_descr list =
  let stmts, sizes = size_of_axes codegen array axes in
  let loopDescriptors = List.map
    (mk_simple_loop_descriptor codegen) sizes in
  stmts, loopDescriptors



let mk_set_val codegen (id:ID.t) (v:SSA.value_node) =
  let impVar = codegen#var id in
  set impVar $ translate_value codegen v

let translate_true_phi_node codegen phiNode =
  let rhs = translate_value codegen phiNode.SSA.phi_left in
  Imp.Set(phiNode.SSA.phi_id, rhs)

let translate_false_phi_node codegen phiNode =
  let rhs = translate_value codegen phiNode.SSA.phi_right in
  Imp.Set(phiNode.SSA.phi_id, rhs)


let rec translate_fn  (ssaFn:SSA.fn) (impInputTypes:ImpType.t list) : Imp.fn =
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
and translate_block (codegen : ImpCodegen.codegen) block : Imp.stmt list =
  Block.fold_forward
    (fun acc stmt -> acc @ (translate_stmt codegen stmt))
    []
    block
and translate_stmt (codegen : ImpCodegen.codegen) stmtNode : Imp.stmt list  =
  match stmtNode.SSA.stmt with
	(* array literals get treated differently from other expressions since*)
	(* they require a block of code rather than simply translating from *)
	(* SSA.exp_node to Imp.exp_node *)
	| SSA.Set([id], {SSA.exp = SSA.Arr elts}) ->
	  translate_array_literal codegen id elts
  (* adverbs get special treatment since they might return multiple values *)
  | SSA.Set(ids, {SSA.exp = SSA.Adverb(adverb, closure, adverbArgs)})->
    let impClosureArgs =
      translate_values codegen closure.SSA.closure_args
    in
    let impInitArgs =
      Option.map (translate_values codegen) adverbArgs.SSA.init
    in
    let impArgs = translate_values codegen adverbArgs.SSA.args in
    let ssaFn =  FnManager.get_typed_function closure.SSA.closure_fn in
    let axes = adverbArgs.SSA.axes in
    translate_adverb
      codegen
      ids
      adverb
      ssaFn
      impClosureArgs
      impInitArgs
      impArgs
      axes
	(* all assignments other than those with an array literal RHS *)
	| SSA.Set([id], rhs) ->
    let impRhs : Imp.value_node = translate_exp codegen rhs in
    let impVar : Imp.value_node = codegen#var id in
    [set impVar impRhs]


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
	  inits @ condBlock @ [Imp.While(condVal, fullBody)]
 | _ ->
   failwith $ Printf.sprintf
     "[Imp_to_SSA] Not yet implemented: %s"
     (SSA.stmt_node_to_str stmtNode)

and translate_exp (codegen:ImpCodegen.codegen) expNode : Imp.value_node  =
  match expNode.SSA.exp with
	| SSA.Values [v] -> translate_value codegen v
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
	  {
      value = Op( opType, op, args');
      value_type = ImpType.ScalarT returnType
    }
	| SSA.PrimApp (Prim.ArrayOp op, args) ->
	  let impArgs = translate_values codegen args in
	  translate_array_op codegen op impArgs
	| SSA.Cast(t, src) ->
	  (* cast only operates on scalar types! *)
	  let eltT = Type.elt_type t in
	  let impT = ImpType.ScalarT eltT in
	  {
      value = Imp.Cast(impT, translate_value codegen src);
      value_type = impT
    }
	| SSA.Arr _ -> failwith "[SSA_to_Imp] Unexpected array expression"
	| _ -> failwith $ "[ssa->imp] unrecognized exp: " ^ (SSA.exp_to_str expNode)

and translate_adverb
      codegen
      (lhsIds:ID.t list)
      (adverb:Prim.adverb)
      (ssaFn:SSA.fn)

      (closureArgs:Imp.value_node list)
      (init:Imp.value_node list option)
      (args:Imp.value_node list)
      (axes:int list) : Imp.stmt list =
  assert (init = None);
  Printf.printf "Closure args: %s, array args: %s\n%!"
    (Imp.value_nodes_to_str closureArgs)
    (Imp.value_nodes_to_str args)
  ;
  let closureArgTypes = List.map Imp.value_type closureArgs in
  let argTypes = List.map Imp.value_type args in
  let num_axes = List.length axes in
  let peeledArgTypes = List.map (ImpType.peel ~num_axes) argTypes in
  let fnInputTypes = closureArgTypes @ peeledArgTypes in
  Printf.printf "Fn input types: %s, given inputs: %s\n%!"
    (Type.type_list_to_str ssaFn.SSA.fn_input_types)
    (ImpType.type_list_to_str fnInputTypes)
  ;
  let nestedFn : Imp.fn = translate_fn ssaFn fnInputTypes in
  match adverb with
  | Prim.Map ->
    translate_map codegen lhsIds nestedFn closureArgs args axes
  | Prim.Reduce ->
    translate_reduce codegen lhsIds nestedFn closureArgs init args axes
   | _ ->
    failwith $ Printf.sprintf
      "[SSA_to_Imp] Unsupported adverb %s"
      (Prim.adverb_to_str adverb)

and translate_map
      (codegen:ImpCodegen.codegen)
      (lhsIds:ID.t list)
      (impFn:Imp.fn)
      (closureArgs:Imp.value_node list)
      (args:Imp.value_node list)
      (axes:int list) : Imp.stmt list  =
  let num_axes : int = List.length axes in
  let bigArray : Imp.value_node = argmax_array_rank args in
  if num_axes <> (ImpType.rank bigArray.Imp.value_type) then
    failwith "Slicing adverbs not yet implemented"
  else begin
    let initBlock, loopDescriptors =
      axes_to_loop_descriptors codegen bigArray axes
    in
    let indices = List.map (fun {loop_var} -> loop_var) loopDescriptors in
    let nestedArrayArgs : Imp.value_node list =
      List.map (fun arg -> ImpHelpers.idx arg indices) args
    in
    let nestedArgs : Imp.value_node list = closureArgs @ nestedArrayArgs in
    let body = [] in
    initBlock @ build_loop_nests codegen loopDescriptors body
  end

and translate_reduce
      (codegen:ImpCodegen.codegen)
      (lhsIds:ID.t list)
      (impFn:Imp.fn)
      (closureArgs:Imp.value_node list)
      (initArgs:Imp.value_node list option)
      (args:Imp.value_node list)
      (axes:int list)  =
  assert false
