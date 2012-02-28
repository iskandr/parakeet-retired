(* pp: -parser o pa_macro.cmo *)

open Base
open Adverb
open Imp
open ImpBuilder
open ImpHelpers
open ImpReplace
open ImpType

(* cache translation for each distinct set of arg types to a function *)
type signature = FnId.t * ImpType.t list
let cache : (signature, Imp.fn) Hashtbl.t = Hashtbl.create 127

(* are these necessary? or should we just register each SSA variable with its *)
(* existing name as an imp variable and then implicitly keep this information *)
(* on the builder object? *)

type loop_descr = {
  loop_var : value_node;
  loop_start : value_node;
  loop_test_val : value_node;
  loop_test_cmp : Prim.scalar_op;
  loop_incr : value_node;
  loop_incr_op : Prim.scalar_op;
}

let get_loop_vars loopDescriptors =
  List.map (fun {loop_var} -> loop_var) loopDescriptors

let rec build_loop_nests
    (builder:ImpBuilder.builder)
    (descrs : loop_descr list)
    (body:Imp.block) =
  match descrs with
	| [] -> body
	| d::ds ->
    let nested = build_loop_nests builder ds body in
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
    (builder:ImpBuilder.builder)
    ?(down=false)
    ?(start=ImpHelpers.zero)
    (stop:Imp.value_node) =
  {
    loop_var = builder#fresh_local ~name:"loop_idx" int32_t;
    loop_start = start;
    loop_test_val = stop;
    loop_test_cmp = (if down then Prim.Gt else Prim.Lt);
    loop_incr = (if down then ImpHelpers.int (-1) else ImpHelpers.one);
    loop_incr_op = Prim.Add;
  }

let translate_value (builder:ImpBuilder.builder) valNode : Imp.value_node =
  match valNode.TypedSSA.value with
  | TypedSSA.Var id -> builder#var id
  | TypedSSA.Num n ->
    {
      value = Imp.Const n;
      value_type = ImpType.ScalarT (ParNum.type_of n)
    }
  | other -> failwith $
      "[ssa->imp] unrecognized value: " ^ (TypedSSA.value_to_str other)

let rec translate_values builder valNodes : Imp.value_node list =
  List.map (translate_value builder) valNodes

let translate_array_op builder (op:Prim.array_op) (args:Imp.value_node list) =
  match op, args with
	| Prim.Index, array::indices ->
	  let arrayT = array.Imp.value_type in
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
      (builder:ImpBuilder.builder)
      (lhsId:ID.t)
      (elts:TypedSSA.value_node list)  =
  let ssaTypes = List.map (fun {TypedSSA.value_type} -> value_type) elts in
  if not (List.for_all Type.is_scalar ssaTypes) then
    failwith "[SSA_to_Imp] Nested arrays not yet implemented"
  else
    let n = List.length elts in
    let firstType = List.hd ssaTypes in
    let impType = ImpType.ArrayT (Type.elt_type firstType, 1) in
    let shape = SymbolicShape.Const n in
    let lhs = builder#var lhsId in
    let impElts : Imp.value_node list = translate_values builder elts in
    let assign_elt idx rhs =
      ImpHelpers.setidx lhs [ImpHelpers.int idx] rhs
    in
    List.mapi assign_elt impElts

(* given an array, map a list of its axes into *)
(* a list of statements and a list of dimsize values *)
let rec size_of_axes
     (builder:ImpBuilder.builder)
     (array:Imp.value_node)
     (axes:Imp.value_nodes) : Imp.block * Imp.value_node list =
  match axes with
  | [] -> [], []
  | axis::rest ->
    let size : Imp.value_node = ImpHelpers.dim array axis in
    let temp = builder#fresh_local ~name:"size" ImpType.int32_t in
    let stmtNode = ImpHelpers.set temp size in
    let restBlock, restVals = size_of_axes builder array rest in
    stmtNode :: restBlock, temp :: restVals

(* given an array and a list of axes, create a list of loop descriptors*)
(* which we can turn into nested loops over the array *)
let rec axes_to_loop_descriptors
    ?(skip_first_iter=false)
    (builder:ImpBuilder.builder)
    (array : Imp.value_node)
    (axes : Imp.value_nodes) : Imp.block * loop_descr list =
  let stmts, sizes = size_of_axes builder array axes in
  let loopDescriptors =
    match  List.map (mk_simple_loop_descriptor builder) sizes with
      | l::ls when skip_first_iter ->
        {l with loop_start = ImpHelpers.one}::ls
      | ls -> ls
  in
  stmts, loopDescriptors

let mk_set_val builder (id:ID.t) (v:TypedSSA.value_node) =
  let impVar = builder#var id in
  set impVar $ translate_value builder v

let translate_true_phi_node builder phiNode =
  let exp = translate_value builder (PhiNode.left phiNode) in
  Imp.Set (PhiNode.id phiNode, exp)

let translate_false_phi_node builder phiNode =
  let exp = translate_value builder (PhiNode.right phiNode) in
  Imp.Set (PhiNode.id phiNode, exp)

let declare_var ssaFn shapeEnv (builder:ImpBuilder.fn_builder) (id, impType)  =
  if List.mem id ssaFn.TypedSSA.input_ids then
    builder#declare_input id impType
  else (
  (* inputs all have the trivial shape
     SHAPE(x) = [dim(x,0), dim(x,1), etc...]
     but outputs and locals actually have non-trivial
     shapes which need to be declared
  *)
    let symShape = ID.Map.find id shapeEnv in
    if List.mem id ssaFn.TypedSSA.output_ids then
      builder#declare_output id ~shape:symShape impType
    else
      builder#declare id ~shape:symShape impType
  )

let rec translate_fn (ssaFn:TypedSSA.fn) (impInputTypes:ImpType.t list) : Imp.fn =
  let signature = ssaFn.TypedSSA.fn_id, impInputTypes in
  match Hashtbl.find_option cache signature with
  | Some impFn ->
    IFDEF DEBUG THEN
      Printf.printf
        "[SSA_to_Imp] Got cached Imp function for %s\n%!"
        (FnId.to_str ssaFn.TypedSSA.fn_id)
        ;
    ENDIF;
    impFn
  | None ->
    let builder = new ImpBuilder.fn_builder in
    let impTyEnv = InferImpTypes.infer ssaFn impInputTypes in
    let shapeEnv : SymbolicShape.env  =
      ShapeInference.infer_normalized_shape_env
        (FnManager.get_typed_function_table ()) ssaFn
    in
    List.iter (declare_var ssaFn shapeEnv builder) (ID.Map.to_list impTyEnv);
    let body =
      translate_block (builder :> ImpBuilder.builder) ssaFn.TypedSSA.body
    in
    let ssa_name = FnId.to_str ssaFn.TypedSSA.fn_id in
    let arg_strings = ImpType.type_list_to_str impInputTypes in
    let name = ssa_name ^ "[" ^ arg_strings ^ "]" in
    let impFn = builder#finalize_fn ~name body in
    Hashtbl.add cache signature impFn;
    IFDEF DEBUG THEN
      Printf.printf
        "[SSA_to_Imp] Created Imp function: %s\n%!"
        (Imp.fn_to_str impFn)
      ;
    ENDIF;
    impFn

and translate_block (builder : ImpBuilder.builder) block : Imp.stmt list =
  Block.fold_forward
    (fun acc stmt -> acc @ (translate_stmt builder stmt))
    []
    block
and translate_stmt (builder : ImpBuilder.builder) stmtNode : Imp.stmt list  =
  match stmtNode.TypedSSA.stmt with
  (* array literals get treated differently from other expressions since*)
  (* they require a block of code rather than simply translating from *)
  (* TypedSSA.exp_node to Imp.exp_node *)
  | TypedSSA.Set([id], {TypedSSA.exp = TypedSSA.Arr elts}) ->
    translate_array_literal builder id elts
  (* adverbs get special treatment since they might return multiple values *)
  | TypedSSA.Set(ids,  {TypedSSA.exp = TypedSSA.Adverb info }) ->
    let impInfo : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info =
      Adverb.apply_to_fields
         info
        ~fn:FnManager.get_typed_function
        ~values:(translate_values builder)
        ~axes:(translate_values builder)
    in
    let lhsValues = List.map builder#var ids in
    translate_adverb builder lhsValues impInfo
	(* all assignments other than those with an array literal RHS *)
  | TypedSSA.Set([id], rhs) ->
    let impRhs : Imp.value_node = translate_exp builder rhs in
    let impVar : Imp.value_node = builder#var id in
    [set impVar impRhs]

  | TypedSSA.Set(ids, {TypedSSA.exp = TypedSSA.Values vs}) ->
    List.map2 (mk_set_val builder) ids vs
  | TypedSSA.Set _ -> failwith "multiple assignment not supported"
  | TypedSSA.SetIdx(lhs, indices, rhs) ->
    let indices : Imp.value_node list = translate_values builder indices in
    let lhs : Imp.value_node = translate_value builder lhs in
    let rhs : Imp.value_node = translate_exp builder rhs in
    [Imp.SetIdx(lhs, indices, rhs)]
  | TypedSSA.If(cond, tBlock, fBlock, phiNodes) ->
    let cond' : Imp.value_node = translate_value builder cond in
    let tBlock' : Imp.block = translate_block builder tBlock in
    let fBlock' : Imp.block = translate_block builder fBlock in
    let trueMerge = List.map (translate_true_phi_node builder) phiNodes in
    let falseMerge = List.map (translate_false_phi_node builder) phiNodes in
    let tBlock' = tBlock' @ trueMerge in
    let fBlock' = fBlock' @ falseMerge in
    [Imp.If(cond', tBlock', fBlock')]
  | TypedSSA.WhileLoop(condBlock, condVal, body, phiNodes) ->
    let inits : Imp.block =
      List.map (translate_true_phi_node builder) phiNodes
    in
    let condBlock : Imp.block  = translate_block builder condBlock in
    let condVal : Imp.value_node = translate_value builder condVal in
    let body : Imp.block = translate_block builder body in
    let finals = List.map (translate_false_phi_node builder) phiNodes in
    let fullBody = body @ finals @ condBlock in
    inits @ condBlock @ [Imp.While(condVal, fullBody)]
 | _ ->
   failwith $ Printf.sprintf
     "[Imp_to_SSA] Not yet implemented: %s"
     (TypedSSA.stmt_node_to_str stmtNode)

and translate_exp (builder:ImpBuilder.builder) expNode : Imp.value_node  =
  match expNode.TypedSSA.exp with
	| TypedSSA.Values [v] -> translate_value builder v
	| TypedSSA.Values _ -> failwith "multiple value expressions not supported"
	| TypedSSA.PrimApp (Prim.ScalarOp op, args) ->
	  let args' = translate_values builder args in
	  let opType, returnType =
	    if Prim.is_comparison op then
	      let firstArg = List.hd args' in
	      ImpType.elt_type firstArg.value_type, Type.BoolT
	    else
	      let retT = Type.elt_type (List.hd expNode.TypedSSA.exp_types) in
	      retT, retT
	  in
	  {
      value = Op(opType, op, args');
      value_type = ImpType.ScalarT returnType
    }
	| TypedSSA.PrimApp (Prim.ArrayOp op, args) ->
	  let impArgs = translate_values builder args in
	  translate_array_op builder op impArgs
	| TypedSSA.Cast(t, src) ->
	  (* cast only operates on scalar types! *)
	  let eltT = Type.elt_type t in
	  let impT = ImpType.ScalarT eltT in
	  {
      value = Imp.Cast(impT, translate_value builder src);
      value_type = impT
    }
	| TypedSSA.Arr _ -> failwith "[SSA_to_Imp] Unexpected array expression"
	| _ ->
    failwith $ Printf.sprintf
      "[ssa->imp] unrecognized exp: %s"
      (TypedSSA.exp_node_to_str expNode)

(* TODO: make this do something sane for adverbs other than Map *)
and translate_adverb
      (builder:ImpBuilder.builder)
      (lhsVars: Imp.value_node list)
      (info : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info)
      : Imp.stmt list =
  let argTypes = List.map Imp.value_type info.array_args in
  let maxArgRank =
    List.fold_left (fun acc t -> max acc (ImpType.rank t)) 0 argTypes
  in
  if maxArgRank = 1 && TypedSSA.ScalarHelpers.is_scalar_fn info.adverb_fn then
    (* only vectorize function which use one type in their body *)
    match TypedSSA.FnHelpers.get_single_type info.adverb_fn with
      | None -> translate_sequential_adverb builder lhsVars info
      | Some (Type.ScalarT eltT) ->
        vectorize_adverb builder lhsVars info eltT
  else translate_sequential_adverb builder lhsVars info

and translate_sequential_adverb
      (builder:ImpBuilder.builder)
      (lhsVars : Imp.value_node list)
      (info : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info)
      : Imp.stmt list =

  (* To implement Map, Scan, and Reduce we loop over the specified axes *)
  (* of the biggest array argument. *)
  (* AllPairs is different in that we loop over the axes of the first arg *)
  (* and nested within we loop over the same set of axes of the second arg. *)
  let slice_along_axes indexVars arr =
    ImpHelpers.idx_or_fixdims arr info.axes indexVars
  in
  (* pick any of the highest rank arrays in the input list *)
  let biggestArray : Imp.value_node = argmax_array_rank info.array_args in
  let initBlock, loopDescriptors, indexVars =
    match info.adverb, info.init with
    | Adverb.Map, None ->
      (* init is a block which gets the dimensions of array we're traversing *)
      let initBlock, loops : (Imp.stmt list) * (loop_descr list) =
        axes_to_loop_descriptors builder biggestArray info.axes
      in
      let indexVars = get_loop_vars loops in
      initBlock, loops, indexVars
    | Adverb.Reduce, None ->
      let initBlock, loops = axes_to_loop_descriptors
          ~skip_first_iter:true builder biggestArray info.axes
      in
      let indexVars = get_loop_vars loops in
      (* the initial value for a reduction is the first elt of the array *)
      let initVal =
        ImpHelpers.idx_or_fixdims
          ~arr:biggestArray
          ~dims:info.axes
          ~indices:(List.map (fun _ -> ImpHelpers.zero) indexVars)
      in
      let initAcc = List.hd lhsVars in
      let copyStmt = ImpHelpers.set initAcc (ImpHelpers.copy initVal) in
      initBlock @ [copyStmt], loops, indexVars
    | Adverb.AllPairs, None ->
      (match info.array_args with
        | [x;y] ->
          let xInit, xLoops = axes_to_loop_descriptors builder x info.axes in
          let xIndexVars = get_loop_vars xLoops in
          let yInit, yLoops = axes_to_loop_descriptors builder y info.axes in
          let yIndexVars = get_loop_vars yLoops in
          xInit@yInit, xLoops@yLoops, xIndexVars@yIndexVars
        | _ -> failwith "allpairs requires two args"
      )
    | _ -> failwith "malformed adverb"

  in
  let nestedArrays = List.map (slice_along_axes indexVars) info.array_args in
  let nestedInputs, nestedOutputs =
    match info.adverb, info.init with
      | Adverb.Map, None
      | Adverb.AllPairs, None ->
        let nestedOutputs = List.map (slice_along_axes indexVars) lhsVars in
        info.fixed_args @ nestedArrays, nestedOutputs
      | Adverb.Reduce, None ->
        info.fixed_args @ lhsVars @ nestedArrays, lhsVars
      | Adverb.Reduce, Some inits ->
        failwith "reduce with inits not implemented"
      | Adverb.Scan, None -> failwith "scan without inits not implemented"
      | Adverb.Scan, Some inits -> failwith "scan with init not implemented"
      | _ -> failwith "malformed adverb"
  in
  let nestedInputTypes = Imp.value_types nestedInputs in
  let impFn : Imp.fn =
    translate_fn info.adverb_fn nestedInputTypes
  in
  let replaceEnv =
    ID.Map.of_lists
      (impFn.input_ids @ impFn.output_ids)
      (nestedInputs @ nestedOutputs)
  in
  let fnBody = ImpReplace.replace_block replaceEnv impFn.body in
  let loops = build_loop_nests builder loopDescriptors fnBody in
  initBlock @ loops

and vectorize_adverb
      (builder : ImpBuilder.builder)
      (lhsVars : Imp.value_node list)
      (adverbInfo : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info)
      (eltT : Type.elt_t) =
  let nbits = Type.sizeof eltT * 8 in
  let biggestArg : Imp.value_node = argmax_array_rank adverbInfo.array_args in
  assert (ImpType.rank biggestArg.value_type = 1);
  (* TODO: actually vectorize here *)
  translate_sequential_adverb builder lhsVars adverbInfo

