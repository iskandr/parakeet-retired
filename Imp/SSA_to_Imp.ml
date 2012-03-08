(* pp: -parser o pa_macro.cmo *)

open Adverb
open Base
open Imp
open ImpBuilder
open ImpHelpers
open ImpReplace
open ImpType
open MachineModel

(* cache translation for each distinct set of arg types to a function *)
type signature = FnId.t * ImpType.t list
let cache : (signature, Imp.fn) Hashtbl.t = Hashtbl.create 127
let vector_cache : (signature, Imp.fn) Hashtbl.t = Hashtbl.create 127

(* For now, we are assuming only 1 CPU *)
let vector_bitwidth = machine_model.cpus.(0).vector_bitwidth

module LoopHelpers = struct
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

  let build_loop_nests
      ?(skip_first_iter = false)
      (outerBlock:ImpBuilder.builder)
      (body: ImpBuilder.builder)
      (descrs:loop_descr list) =
    let rec aux currBlock = function
      | [] -> body
      | d::ds ->
        (* if this is this innermost loop iteration, *)
        (* and we're supposed to skip the first iteration, *)
        (* then initialize to "initidx" *)
        if ds = [] && skip_first_iter then (
          let init =
            outerBlock#build_add "initidx" d.loop_start d.loop_incr
          in
          currBlock += Set(d.loop_var, init);
          currBlock += Set(init, d.loop_start);
        )
        else currBlock += Set(d.loop_var, d.loop_start)
        ;
        let nestedBlock = new builder currBlock#info in
        aux nestedBlock ds;
        let loopVarT = d.loop_var.value_type in
        let next = ImpHelpers.scalar_op d.loop_incr_op d.loop_var d.loop_incr in
        nestedBlock += Set(d.loop_var, nest);
        let test = ImpHelpers.cmp d.loop_test_cmp d.loop_var d.loop_test_val in
        currBlock += While(test, to_list nestedBlock)
    in
    aux outerBlock descrs

(* TODO: *)
(* - Get rid of SetIdx*)
(* - change Set to take a value_node lhs *)
(* - Fix up mli for ImpBuilder *)
(* - get rid of ImpHelpers and old objects *)
(* - move inline into ImpBuilder *)
(* - merge idx and fixdims, move them to ImpBuilder *)

  let mk_simple_loop_descriptor
      (block : ImpBuilder.builder)
      ?(down=false)
      ?(start=ImpHelpers.zero)
      (stop:Imp.value_node) =
    {
      loop_var = fresh_local "loop_idx" int32_t block;
      loop_start = start;
      loop_test_val = stop;
      loop_test_cmp = (if down then Prim.Gt else Prim.Lt);
      loop_incr = (if down then ImpHelpers.int (-1) else ImpHelpers.one);
      loop_incr_op = Prim.Add;
    }

  (* given an array, map a list of its axes into *)
  (* a list of statements and a list of dimsize values *)
  let rec size_of_axes
      (builder:ImpBuilder.builder)
      (array:Imp.value_node)
      (axes:Imp.value_nodes) :  Imp.value_node list =
    match axes with
    | [] ->  []
    | axis::rest ->
      let size : Imp.value_node = ImpHelpers.dim array axis in
      let temp = fresh_local "size" ImpType.int32_t builder in
      builder += Set(temp, size);
      temp :: size_of_axes builder array rest

  (* given an array and a list of axes, create a list of loop descriptors *)
  (* which we can turn into nested loops over the array *)
  let rec axes_to_loop_descriptors
      ?(skip_first_iter=false)
      (builder:ImpBuilder.builder)
      (array:Imp.value_node)
      (axes:Imp.value_nodes) : Imp.block * loop_descr list =
    let stmts, sizes = size_of_axes builder array axes in
    let loopDescriptors = List.map (mk_simple_loop_descriptor builder) sizes in
    if skip_first_iter then
      (* modify innermost descriptor if need to skip its first iteration *)
      match List.rev loopDescriptors with
      | last::rest ->
        stmts, List.rev ({last with loop_start = ImpHelpers.one}::rest)
      | [] -> stmts, []
    else stmts, loopDescriptors
end
open LoopHelpers



let rec idx_or_fixdims
    ~(arr:value_node)
    ~(dims:value_nodes)
    ~(indices:value_nodes) : value_node =
  let nIndices = List.length indices in
  let arrT = arr.value_type in
  let rank = ImpType.rank arrT in
  IFDEF DEBUG THEN
    let nDims = List.length dims in
    if nDims <> nIndices then
      failwith $ Printf.sprintf
        "[idx_or_fixdims] Mismatch between # of dims (%d) and # of indices(%d)"
        nDims
        nIndices
    ;
    if (nIndices > rank) && (rank > 0) then
      failwith $ Printf.sprintf
        "[idx_or_fixdims] Can't index into rank %d array with %d indices"
        rank
        nIndices
    ;
  ENDIF;
  (* for convenience, treat indexing into scalars as the identity operation *)
  if rank = 0 then  arr
  else if rank = nIndices && (List.for_all ImpHelpers.is_const_int dims) then
    idx arr ~dims indices
  else
    fixdims arr dims indices

let copy (builder:ImpBuilder.builder) ~from_array ~to_array =
  let toType = to_array.value_type in
  let toRank = ImpType.rank toType in
  IFDEF DEBUG THEN
    let fromType = from_array.value_type in
    let fromRank = ImpType.rank fromType in
    (* allow from_array to be a scalar *)
    if fromRank > 0 && toRank <> fromRank then
      failwith $ Printf.sprintf
            "[SSA_to_Imp] Can't copy from %s (rank=%d) to %s (rank=%d)"
            (ImpType.to_str fromType)
            fromRank
            (ImpType.to_str toType)
            toRank
  ENDIF;
  let axes = ImpHelpers.ints_til toRank in
  let loopDescriptors = axes_to_loop_descriptors builder to_array axes in
  let indexVars = get_loop_vars loopDescriptors in
  let lhs = idx to_array ~dims:axes indexVars in
  let rhs = idx from_array ~dims:axes indexVars in
  let body = new builder builder#info in
  body += Set(lhs, rhs);
  build_loop_nests builder loopDescriptors body

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
    let resultT = ImpType.elt_type arrayT in
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
    (elts:TypedSSA.value_node list) =
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
      builder += Imp.Set(ImpBuilder.idx lhs [int idx], rhs)
    in
    List.iteri assign_elt impElts

let mk_set_val builder (id:ID.t) (v:TypedSSA.value_node) =
  Set (builder#var id, translate_value builder v)

let translate_true_phi_node builder phiNode =
  let exp = translate_value builder (PhiNode.left phiNode) in
  Imp.Set (builder#var (PhiNode.id phiNode), exp)

let translate_false_phi_node builder phiNode =
  let exp = translate_value builder (PhiNode.right phiNode) in
  Imp.Set (builder#var (PhiNode.id phiNode), exp)

let declare_input (fnBuilder:ImpBuilder.fn_builder) typeEnv id =
  let impType = ID.Map.find id typeEnv in
  fnBuilder#declare_input id impType

let declare_output (fnBuilder:ImpBuilder.fn_builder) shapeEnv typeEnv id =
  let impType = ID.Map.find id typeEnv in
  let shape = ID.Map.find id shapeEnv in
  fnBuilder#declare_output id ~shape impType

let declare_local_var (builder:ImpBuilder.builder) nonlocals shapes storages (id, t) =
  if not (List.mem id nonlocals) then (
    let shape = ID.Map.find id shapes in
    let storage = ID.Map.find id storages in
    declare id ~shape ~storage t builder
  )


let rec translate_fn (ssaFn:TypedSSA.fn) (impInputTypes:ImpType.t list)
    : Imp.fn =
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
    IFDEF DEBUG THEN
      Printf.printf "[SSA_to_Imp] Translating \n%s\n with args: %s\n"
        (TypedSSA.fn_to_str ssaFn)
        (ImpType.type_list_to_str impInputTypes)
    ENDIF;
    let name = (FnId.to_str ssaFn.TypedSSA.fn_id) ^ "[" ^ arg_strings ^ "]" in
    let fnBuilder = new ImpBuilder.fn_builder name in
    let impTyEnv = InferImpTypes.infer ssaFn impInputTypes in
    let shapeEnv : SymbolicShape.env =
      ShapeInference.infer_normalized_shape_env
        (FnManager.get_typed_function_table ()) ssaFn
    in
    let storageEnv : Imp.storage ID.Map.t = InferImpStorage.infer ssaFn in
    let inputIds = ssaFn.TypedSSA.input_ids in
    let outputIds = ssaFn.TypedSSA.output_ids in
    List.iter (declare_input fnBuilder impTyEnv) inputIds;
    List.iter (declare_output fnBuilder shapeEnv impTyEnv) outputIds;
    let nonlocals = inputIds @ outputIds in
    let bodyBuilder : ImpBuilder.builder = builder#body in
    let () =
      List.iter
        (declare_local_var bodyBuilder nonlocals shapeEnv storageEnv)
        (ID.Map.to_list impTyEnv)
    in
    translate_block bodyBuilder ssaFn.TypedSSA.body;
    let arg_strings = ImpType.type_list_to_str impInputTypes in
    let impFn = builder#finalize_fn in
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
  IFDEF DEBUG THEN
    Printf.printf "[SSA_to_Imp.translate_stmt] %s\n"
      (TypedSSA.stmt_node_to_str stmtNode)
  ENDIF;
  match stmtNode.TypedSSA.stmt with
  (* array literals get treated differently from other expressions since *)
  (* they require a block of code rather than simply translating from *)
  (* TypedSSA.exp_node to Imp.exp_node *)
  | TypedSSA.Set([id], {TypedSSA.exp = TypedSSA.Arr elts}) ->
    translate_array_literal builder id elts
  (* adverbs get special treatment since they might return multiple values *)
  | TypedSSA.Set(ids, {TypedSSA.exp = TypedSSA.Adverb info}) ->
    let impInfo : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info =
      Adverb.apply_to_fields
         info
        ~fn:FnManager.get_typed_function
        ~values:(translate_values builder)
        ~axes:(translate_values builder)
    in
    let lhsValues = List.map (fun id -> builder#var id) ids in
    translate_adverb builder lhsValues impInfo
  (* all assignments other than those with an array literal RHS *)
  | TypedSSA.Set([id], rhs) ->
    let impRhs : Imp.value_node = translate_exp builder rhs in
    let impVar : Imp.value_node = builder#var id in
    builder += Set(impVar, impRhs)
  | TypedSSA.Set(ids, {TypedSSA.exp = TypedSSA.Values vs}) ->
    List.map2 (mk_set_val builder) ids vs

  | TypedSSA.Set _ -> failwith "multiple assignment not supported"
  | TypedSSA.SetIdx(arr, indices, rhs) ->
    let indices : Imp.value_node list = translate_values builder indices in
    let arr : Imp.value_node = translate_value builder lhs in
    let rhs : Imp.value_node = translate_exp builder rhs in
    builder += Set(idx arr indices, rhs)
  | TypedSSA.If(cond, tBlock, fBlock, phiNodes) ->
    let cond' = translate_value builder cond in
    let trueBuilder = builder#clone in
    let falseBuilder = builder#clone in
    translate_block trueBuilder tBlock;
    List.iter (translate_true_phi_node trueBuilder) phiNodes;
    translate_block falseBuilder fBlock;
    List.iter (translate_false_phi_node falseBuilder) phiNodes;
    builder += If(cond', trueBuilder#to_list , falseBuilder#to_list)
  | TypedSSA.WhileLoop(condBlock, condVal, body, phiNodes) ->
    (* initialize with true branches of phi nodes *)
    List.iter (translate_true_phi_node builder) phiNodes;
    (* translate code to compute condition *)
    translate_block builder condBlock;
    let condVal : Imp.value_node = translate_value condBuilder condVal in
    let bodyBuilder = builder#clone in
    translate_block bodyBuilder body;
    List.iter (translate_false_phi_node bodyBuilder) phiNodes;
    translate_block bodyBuilder condBlock;
    builder += Imp.While(condVal,  bodyBuilder#to_list)
 | _ ->
   failwith $ Printf.sprintf
     "[Imp_to_SSA] Not yet implemented: %s"
     (TypedSSA.stmt_node_to_str stmtNode)

and translate_exp (builder:ImpBuilder.builder) expNode : Imp.value_node  =
  IFDEF DEBUG THEN
    Printf.printf "[SSA_to_Imp.translate_exp] %s\n"
      (TypedSSA.exp_node_to_str expNode)
  ENDIF;
  match expNode.TypedSSA.exp with
  | TypedSSA.Values [v] -> translate_value builder v
  | TypedSSA.Values _ -> failwith "multiple value expressions not supported"
  | TypedSSA.PrimApp(Prim.ScalarOp op, args) ->
    let args' = translate_values builder args in
    let opType, returnType =
      if Prim.is_comparison op then
        let firstArg = List.hd args' in
        firstArg.value_type, ImpType.bool_t
      else
        let retT =
          ImpType.ScalarT (Type.elt_type (List.hd expNode.TypedSSA.exp_types))
        in
        retT, retT
    in
    {
      value = Op(opType, op, args');
      value_type = returnType
    }
  | TypedSSA.PrimApp(Prim.ArrayOp op, args) ->
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

and inline (builder:ImpBuilder.builder) impFn inputs outputs : Imp.block =
  let rename_local oldId =
    let name = ID.get_original_prefix oldId in
    let t = ID.Map.find oldId impFn.types in
    let shape = ID.Map.find oldId impFn.shapes in
    let storage = ID.Map.find oldId impFn.storage in
    builder#fresh_local name ~storage ~shape t
  in
  let newLocalVars = List.map rename_local impFn.local_ids in
  let replaceEnv =
    ID.Map.of_lists
      (impFn.input_ids @ impFn.output_ids @ impFn.local_ids)
      (inputs @ outputs @ newLocalVars)
  in
  ImpReplace.replace_block replaceEnv impFn.body

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
  if maxArgRank = 1 &&
     TypedSSA.ScalarHelpers.is_scalar_fn info.adverb_fn &&
     info.adverb = Adverb.Map
  then match TypedSSA.FnHelpers.get_single_type info.adverb_fn with
    | None -> translate_sequential_adverb builder lhsVars info
    | Some (Type.ScalarT eltT) ->
      (* only vectorize function which use one type in their body *)
      vectorize_adverb builder lhsVars info eltT
  else translate_sequential_adverb builder lhsVars info

and translate_sequential_adverb
    (builder:ImpBuilder.builder)
    (lhsVars : Imp.value_node list)
    (info : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info)
    : Imp.stmt list =
  IFDEF DEBUG THEN
    Printf.printf "[SSA_to_Imp.sequential adverb] %s%!\n"
      (Adverb.info_to_str
        info
        TypedSSA.PrettyPrinters.fn_id_to_str
        Imp.value_nodes_to_str
        Imp.value_nodes_to_str
      )
  ENDIF;
  let slice_one_along_axes indexVars arr =
    builder#fixdims arr info.axes indexVars
  in
  (* pick any of the highest rank arrays in the input list *)
  let biggestArray : Imp.value_node = argmax_array_rank info.array_args in
  (* To implement Map, Scan, and Reduce we loop over the specified axes *)
  (* of the biggest array argument. AllPairs is different in that we loop *)
  (* over the axes of the first arg and nested within we loop over the *)
  (* same set of axes of the second arg. *)
  let nAxes = List.length info.axes in
  let nestedBuilder = new builder (builder#info) in
  let loopDescriptors, nestedInputs, nestedOutputs, skipFirstIter =
    match info.adverb, info.init with
    | Adverb.Map, None ->
      (* init is a block which gets the dimensions of array we're traversing *)
      let loops = axes_to_loop_descriptors builder biggestArray info.axes in
      let indexVars = get_loop_vars loops in
      let nestedArrays = slice_along_axes indexVars info.array_args in
      let nestedInputs = info.fixed_args @ nestedArrays in
      let nestedOutputs = slice_along_axes indexVars lhsVars in
      loops, nestedInputs, nestedOutputs, false
    | Adverb.Reduce, None ->
      let loops = axes_to_loop_descriptors builder biggestArray info.axes in
      let indexVars = get_loop_vars loops in
      let zeroIndices = List.map (fun _ -> ImpHelpers.zero) indexVars in
      (* the initial value for a reduction is the first elt of the array *)
      let initVal =
        idx_or_fixdims ~arr:biggestArray ~dims:info.axes ~indices:zeroIndices
      in
      copy builder ~from_array:initVal ~to_array:(List.hd lhsVars);
      (* TODO: where do the fixdim statements go for nested arrays? *)
      let nestedArrays =
        List.map (slice_along_axes indexVars) info.array_args
      in
      let nestedInputs = info.fixed_args @ lhsVars @ nestedArrays in
      loops, nestedInputs, lhsVarss, true
    | Adverb.AllPairs, None ->
      (match info.array_args with
        | [x;y] ->
          let xLoops = axes_to_loop_descriptors builder x info.axes in
          let xIndexVars = get_loop_vars xLoops in
          let yLoops = axes_to_loop_descriptors builder y info.axes in
          let yIndexVars = get_loop_vars yLoops in
          let nestedArrays =
            [slice_along_axes xIndexVars x; slice_along_axes yIndexVars y]
          in

          let constOutputAxes = List.til (2*List.length info.axes) in
          let outputAxes = List.map ImpHelpers.int constOutputAxes in
          let nestedOutputs =
          List.map (fun arr -> idx_or_fixdims arr outputAxes indexVars) lhsVars
          in
          let nestedInputs = info.fixed_args @ nestedArrays in
          xLoops@yLoops, nestedInputs, nestedOutputs, false
        | _ -> failwith "allpairs requires two args"
      )
    | _ -> failwith "malformed adverb"
  in
  let nestedInputTypes = Imp.value_types nestedInputs in
  let impFn : Imp.fn =
    translate_fn info.adverb_fn nestedInputTypes
  in
  let nestedBody = inline builder impFn nestedInputs nestedOutputs in
  let loops =
    build_loop_nests
      ~skip_first_iter:skipFirstIter builder loopDescriptors nestedBody
  in
  initBlock @ loops

(* We assume that the function is a scalar function at this point *)
and vectorize_adverb
    (builder : ImpBuilder.builder)
    (lhsVars : Imp.value_node list)
    (info : (TypedSSA.fn, Imp.value_nodes, Imp.value_nodes) Adverb.info)
    (eltT : ImpType.elt_t) : Imp.stmt list =
  IFDEF DEBUG THEN
    Printf.printf "[SSA_to_Imp.vectorize_adverb] %s with elt_t = %s\n"
      (Adverb.info_to_str
        info
        TypedSSA.PrettyPrinters.fn_id_to_str
        Imp.value_nodes_to_str
        Imp.value_nodes_to_str
      )
      (Type.elt_to_str eltT)
    ;
  ENDIF;
  let fixedTypes = List.map Imp.value_type info.fixed_args in
  let argTypes = List.map Imp.value_type info.array_args in
  let num_axes = List.length info.axes in
  let peeledArgTypes = List.map (ImpType.peel ~num_axes) argTypes in
  let fnInputTypes = fixedTypes @ peeledArgTypes in
  let impFn = translate_fn info.adverb_fn fnInputTypes in
  match info.adverb with
  | Adverb.Map ->
    (* TODO: for now, only vectorize maps *)
    (* Get the biggest array, and then create outer loops for all but the *)
    (* innermost axis.  That axis is treated differently so as to be *)
    (* vectorized. *)
    let biggestArray : Imp.value_node = argmax_array_rank info.array_args in
    let num_axes = List.length info.axes in
    let outerInit, outerLoops : (Imp.stmt list) * (loop_descr list) =
      axes_to_loop_descriptors
        builder biggestArray (List.take (num_axes - 1) info.axes)
    in
    let outerIndexVars = get_loop_vars outerLoops in
    (* Vectorize the innermost axis. *)
    let lastAxis = List.hd (List.rev info.axes) in
    let [lastInit], [lastSize] = size_of_axes builder biggestArray [lastAxis] in
    let vecLen = vector_bitwidth / (8 * (Type.sizeof eltT)) in
    let impVecLen = ImpHelpers.int vecLen in
    let impVecLoopBound = fresh_local "vec_loop_bound" int32_t builder in
    let vecInit = [
      lastInit;
      ImpHelpers.set
        impVecLoopBound (ImpHelpers.sub ~t:Type.Int32T lastSize impVecLen);
      ImpHelpers.set
        impVecLoopBound
        (ImpHelpers.div ~t:Type.Int32T impVecLoopBound impVecLen);
      ImpHelpers.set
        impVecLoopBound
        (ImpHelpers.mul ~t:Type.Int32T impVecLoopBound impVecLen);
      ImpHelpers.set
        impVecLoopBound
        (ImpHelpers.add ~t:Type.Int32T impVecLoopBound ImpHelpers.one)
    ]
    in
    let vecDescriptor =
      {
        loop_var = fresh_local "vec_loop_idx" int32_t builder;
        loop_start = ImpHelpers.zero;
        loop_test_val = impVecLoopBound;
        loop_test_cmp = Prim.Lt;
        loop_incr = impVecLen;
        loop_incr_op = Prim.Add;
      }
    in
    let vecIndexVars = outerIndexVars @ [vecDescriptor.loop_var] in
    let vecSliceArgs =
      List.map
        (fun arg -> ImpHelpers.vec_slice arg vecLen vecIndexVars)
        info.array_args
    in
    let vecNestedArgs = info.fixed_args @ vecSliceArgs in
    let vecOutputs =
      List.map (fun arg -> ImpHelpers.vec_slice arg vecLen vecIndexVars) lhsVars
    in
    let vecFn = Imp_to_SSE.vectorize_fn impFn vecLen in
    let vecFnBody =  inline builder vecFn vecNestedArgs vecOutputs in
    let vecLoop = build_loop_nests builder [vecDescriptor] vecFnBody in

    (* Add loop to handle straggler elements that can't be vectorized *)
    (* TODO: check whether we want to add this loop based on shape? *)
    let seqDescriptor =
      {
        loop_var = fresh_local "seq_loop_idx" int32_t builder;
        loop_start = impVecLoopBound;
        loop_test_val = lastSize;
        loop_test_cmp = Prim.Lt;
        loop_incr = ImpHelpers.one;
        loop_incr_op = Prim.Add;
      }
    in
    let seqIndexVars = outerIndexVars @ [seqDescriptor.loop_var] in
    let seqArrayArgs =
      List.map (fun arg -> ImpHelpers.idx arg seqIndexVars) info.array_args
    in
    let seqNestedArgs = info.fixed_args @ seqArrayArgs in
    let seqNestedOutputs =
      List.map (fun arg -> ImpHelpers.idx arg seqIndexVars) lhsVars
    in
    let seqFnBody = inline builder impFn seqNestedArgs seqNestedOutputs in
    let seqLoop = build_loop_nests builder [seqDescriptor] seqFnBody in

    (* Build the outer loops, injecting the vectorized and sequential inner *)
    (* loops.  Then return the vectorized block. *)
    let vecLoops = build_loop_nests builder outerLoops (vecLoop @ seqLoop) in
    outerInit @ vecInit @ vecLoops
  | _ -> translate_sequential_adverb builder lhsVars info
