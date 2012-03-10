(* pp: -parser o pa_macro.cmo *)

open Base
open PhiNode
open Adverb
open Printf
open Type

exception RewriteFailed of string * SrcInfo.t option

module type REWRITE_PARAMS = sig
  val specializer : UntypedSSA.value -> Signature.t -> TypedSSA.fn
  val tenv : (ID.t, Type.t) Hashtbl.t
end

module Make(P: REWRITE_PARAMS) = struct
  let get_type id =
    match Hashtbl.find_option P.tenv id with
      | Some t -> t
      | None ->
        raise $ RewriteFailed ("No type for " ^ (ID.to_str id), None)

  let set_type id t = Hashtbl.replace P.tenv id t

  let fresh_id ?(name="temp") t =
    let id = ID.gen_named name in
    set_type id t;
    id

  let annotate_value (valNode:UntypedSSA.value_node) : TypedSSA.value_node =
    let src = valNode.UntypedSSA.value_src in
    match valNode.UntypedSSA.value with
      | UntypedSSA.Var id ->
        {
          TypedSSA.value = TypedSSA.Var id;
          value_type = get_type id;
          value_src = src
       }
      | UntypedSSA.Num n ->
        {
          TypedSSA.value = TypedSSA.Num n;
          value_type = Type.ScalarT (ParNum.type_of n);
          value_src = src
        }
      | other ->
        let errMsg = Printf.sprintf
          "Don't know how to convert %s into typed SSA"
          (UntypedSSA.value_to_str other)
        in
        raise (RewriteFailed(errMsg, src))

  let annotate_values vs = List.map annotate_value vs

  let rewrite_typed_value (t:Type.t) (typedValNode:TypedSSA.value_node) =
    let src = typedValNode.TypedSSA.value_src in
    match typedValNode.TypedSSA.value with
      | TypedSSA.Num n ->
        let n' = ParNum.coerce n (Type.elt_type t) in
        TypedSSA.num ?src:typedValNode.TypedSSA.value_src n'
      | TypedSSA.Var id ->
        let t' = get_type id in
        if t = t' then {typedValNode with TypedSSA.value_type = t }
        else
          let errMsg =
            Printf.sprintf
              "Cannot rewrite %s : %s to become %s"
              (ID.to_str id)
              (Type.to_str t')
              (Type.to_str t)
          in raise (RewriteFailed(errMsg, src))


  (* rewrite a value to have type t, but can't create coercion statements
     for variables of the wrong type
   *)
  let rewrite_value (t:Type.t) (valNode:UntypedSSA.value_node) =
    let typedValNode : TypedSSA.value_node = annotate_value valNode in
    if typedValNode.TypedSSA.value_type = t then typedValNode
    else rewrite_typed_value t typedValNode

  (* if a value needs to be coerced, then it gets added to this list.
     It's the job of any caller to check and clear this list
   *)
  let coercions = ref []

  let add_coercion ?src expNode =
    let t = List.hd expNode.TypedSSA.exp_types in
    let id = fresh_id ~name:"coerce" t in
    let stmtNode = TypedSSA.set [id] expNode in
    coercions := stmtNode :: !coercions;
    TypedSSA.var ?src t id

  let get_coercions () = !coercions

  let clear_coercions () = coercions := []

  let collect_coercions ()  =
    let stmts = get_coercions()  in
    clear_coercions ();
    stmts

  let coerce_typed_value (t:Type.t) (simpleTyped:TypedSSA.value_node) =
    if simpleTyped.TypedSSA.value_type = t then simpleTyped
    else match simpleTyped.TypedSSA.value with
    | TypedSSA.Var id ->
      let coerceExp = TypedSSA.cast t simpleTyped in
      let src = simpleTyped.TypedSSA.value_src in
      add_coercion ?src coerceExp
    | _ -> rewrite_typed_value t simpleTyped

  let coerce_typed_values t vs = List.map (coerce_typed_value t) vs


  let coerce_value (t:Type.t) (untyped:UntypedSSA.value_node) =
    coerce_typed_value t (annotate_value untyped)

  let coerce_values t vs = List.map (coerce_value t) vs

  let untyped_value_input_arity = function
    | UntypedSSA.GlobalFn fnId -> FnManager.input_arity_of_untyped_fn fnId
    | UntypedSSA.Prim p -> Prim.min_prim_arity p
    | other ->
      failwith $ Printf.sprintf
        "Can't get arity of %s, it's not a function"
        (UntypedSSA.value_to_str other)

  let untyped_value_output_arity = function
    | UntypedSSA.GlobalFn fnId ->
      FnManager.output_arity_of_untyped_fn fnId
    | UntypedSSA.Prim p -> 1
    | other ->
      failwith $ Printf.sprintf
        "Can't get arity of %s, it's not a function"
        (UntypedSSA.value_to_str other)

  let rewrite_adverb
        ?(src:SrcInfo.t option)
        (info : UntypedSSA.adverb_info) : TypedSSA.exp_node =
    (* make typed version of all the adverbinfo fields *)
    let arrayArgs : TypedSSA.value_nodes = annotate_values info.array_args in
    let arrayTypes = TypedSSA.types_of_value_nodes arrayArgs in
    let fixed : TypedSSA.value_nodes = annotate_values info.fixed_args in
    let fixedTypes = TypedSSA.types_of_value_nodes fixed in
    let init = Option.map annotate_values info.init in
    let axes : TypedSSA.value_nodes = match info.axes with
      | Some axes -> annotate_values axes
      | None -> AdverbHelpers.infer_adverb_axes_from_types arrayTypes
    in
    let numAxes = List.length axes in
    let eltTypes = List.map (Type.peel ~num_axes:numAxes) arrayTypes in
    (* cut down on code repetition since so many fields are shared below *)
    let mk_adverb_exp (adverb:Adverb.t) (typedFn:TypedSSA.fn) =
      AdverbHelpers.mk_adverb_exp_node ?src
        {
          Adverb.adverb= adverb;
          adverb_fn = typedFn.TypedSSA.fn_id;
          fixed_args = fixed;
          init = init;
          axes = axes;
          array_args = arrayArgs;
        }
    in
    let untypedFnVal = info.adverb_fn.UntypedSSA.value in
    match info.adverb, init with
      | Adverb.AllPairs, None
      | Adverb.Map, None  ->
        let nestedSig = Signature.from_input_types (fixedTypes @ eltTypes) in
        Printf.printf "Specializing map for argument types %s\n%!"
          (Signature.to_str nestedSig);
        let typedFn = P.specializer untypedFnVal nestedSig in
        mk_adverb_exp info.adverb typedFn
      | Adverb.Map, Some _ -> failwith "Map can't have initial args"
      | Adverb.AllPairs, None ->
        failwith "AllPairs operator must have two inputs"
      | Adverb.AllPairs, Some _ ->
        failwith "AllPairs operator can't have initial args"
      | Adverb.Reduce, None ->
        if List.length eltTypes <> 1 then
          failwith "Reduce without initial args can only have 1 input"
        else
        let eltT = List.hd eltTypes in
        let inputArity = untyped_value_input_arity untypedFnVal in
        if inputArity <> 2 then
          let errMsg =
            Printf.sprintf
              "Reduce without init requires binary operator, given %s (arity %d"
              (UntypedSSA.PrettyPrinters.value_to_str untypedFnVal)
              inputArity
          in
          raise $ RewriteFailed(errMsg, src)
        else
        (* assume that the accumulator is the same as the array element type *)
        let nestedSig =
          Signature.from_input_types (fixedTypes @ [eltT; eltT])
        in
        let typedFn = P.specializer untypedFnVal nestedSig in
        mk_adverb_exp Adverb.Reduce typedFn
      | _ -> failwith "Malformed adverb"


  let rewrite_array_op
        (src: SrcInfo.t option)
        (op:Prim.array_op)
        (args:TypedSSA.value_nodes)
        (types:Type.t list) = match op, args, types with
    | Prim.Index, [array; index], [arrayType; indexType]
        when Type.elt_type indexType = Type.BoolT ->
        IFDEF DEBUG THEN
            if Type.rank indexType <> 1 then
              failwith "Expected boolean index vector to be 1D"
        ENDIF;
        let whereT = Type.ArrayT(Type.Int32T, 1) in
        let whereOp =  Prim.ArrayOp Prim.Where in
        let whereExp =
          TypedSSA.primapp ?src whereOp ~output_types:[whereT] [index]
        in
        let whereResult = add_coercion ?src whereExp in
        let args' = array :: [whereResult] in
        let resultType = Type.ArrayT (Type.elt_type arrayType, 1) in
        let indexOp = Prim.ArrayOp Prim.Index in
        TypedSSA.primapp ?src indexOp ~output_types:[resultType] args'
    | _ ->
        let outT = TypeAnalysis.infer_simple_array_op op types in
        TypedSSA.primapp ?src (Prim.ArrayOp op) ~output_types:[outT] args

  let rewrite_app src
        (fn:UntypedSSA.value_node)
        (argNodes:TypedSSA.value_nodes) : TypedSSA.exp_node =
    IFDEF DEBUG THEN
      Printf.printf "[RewriteTyped.rewrite_app] %s(%s)\n"
        (UntypedSSA.value_node_to_str fn)
        (TypedSSA.value_nodes_to_str argNodes)
      ;
    ENDIF;
    let argTypes = List.map (fun v -> v.TypedSSA.value_type) argNodes in
    let fnVal = fn.UntypedSSA.value in
    let fnSrc = fn.UntypedSSA.value_src in
    match fnVal with
    | UntypedSSA.Prim ((Prim.ScalarOp op) as p) ->
      let outT = TypeAnalysis.infer_scalar_op op argTypes in
      if Type.is_array outT then
        let axes =
          AdverbHelpers.infer_adverb_axes_from_types argTypes
        in
        let eltTypes =
          List.map (fun t -> Type.ScalarT (Type.elt_type t)) argTypes
        in
        let typedFn =
          P.specializer fnVal (Signature.from_input_types eltTypes)
        in
        let typedInfo : TypedSSA.adverb_info =
          {
            Adverb.adverb = Adverb.Map;
            adverb_fn = typedFn.TypedSSA.fn_id;
            fixed_args = [];
            init = None;
            array_args = argNodes;
            axes = axes;
          }
        in
        AdverbHelpers.mk_adverb_exp_node typedInfo

      else
        (* most scalar ops expect all their arguments to be of the same*)
        (* type, except for Select, whose first argument is always a bool *)
        begin
          match op, argNodes, argTypes with
          | Prim.Select, boolArg::otherArgs, _::otherTypes ->
            let inT = Type.combine_type_list otherTypes in
            let args =
              (coerce_typed_value Type.bool boolArg)
              ::
              (coerce_typed_values inT argNodes)
            in
            TypedSSA.primapp p [outT] args
          | _ ->
            (* if operation is a float, then make sure the inputs are*)
            (* at least float32 *)
            let inT =
              if Prim.is_float_op op then
                Type.combine_type_list (Type.float32::argTypes)
              else
                Type.combine_type_list argTypes
            in
            let args = coerce_typed_values inT argNodes in
            IFDEF DEBUG THEN
              Printf.printf "PRIMAPP: %s %s=>%s for inputs %s"
                (Prim.to_str p)
                (Type.to_str inT)
                (Type.to_str outT)
                (TypedSSA.value_nodes_to_str argNodes)
            ENDIF;
            TypedSSA.primapp p [outT] args
        end
    | UntypedSSA.Prim (Prim.ArrayOp op) ->
      rewrite_array_op src op argNodes argTypes
    | UntypedSSA.Prim (Prim.Adverb adverb) ->
      raise (RewriteFailed(
        "Unexpected adverb, should have been handled in rewrite_exp", fnSrc))


    | UntypedSSA.GlobalFn _ ->
      let typed = P.specializer fnVal (Signature.from_input_types argTypes) in
      let outputTypes = TypedSSA.output_types typed in
      TypedSSA.call ?src typed.TypedSSA.fn_id outputTypes argNodes
    | UntypedSSA.Var id ->
      (* assume variable must be an array *)
      let arrType = Hashtbl.find P.tenv id in
      let outTypes = [Type.peel ~num_axes:(List.length argNodes) arrType] in
      let arrNode = TypedSSA.var ?src:fnSrc arrType id in
      let indexOp = Prim.ArrayOp Prim.Index in
      TypedSSA.primapp ?src indexOp  ~output_types:outTypes (arrNode::argNodes)

    | _ ->
      let errMsg =
        Printf.sprintf "[RewriteTyped] Unexpected function: %s"
          (UntypedSSA.value_node_to_str fn)
      in
      raise (RewriteFailed(errMsg, fnSrc))

  let rewrite_exp types expNode =
    IFDEF DEBUG THEN
      Printf.printf "[RewriteTyped.exp] %s :: %s\n%!"
        (UntypedSSA.PrettyPrinters.exp_node_to_str expNode)
        (Type.type_list_to_str types)
    ENDIF;
    UntypedSSA.(
      let src = expNode.exp_src in
      match expNode.exp, types with
      | Arr elts, [Type.ArrayT(eltT, _)] ->
        let elts' = coerce_values (Type.ScalarT eltT) elts in
        { TypedSSA.exp = TypedSSA.Arr elts'; exp_types = types; exp_src = src}
      | Values vs, _ ->
        let vs' = List.map2 coerce_value types vs in
        { TypedSSA.exp = TypedSSA.Values vs'; exp_types = types; exp_src = src }
        (* WARNING: You're ignoring the expected return types here *)
      | Adverb adverbInfo, _ -> rewrite_adverb ?src adverbInfo
      | App ({value = Prim (Prim.Adverb adverb)}, fn::args), _ ->
        IFDEF DEBUG THEN
          Printf.printf "[RewriteTyped] Converting simple adverb\n%!";
        ENDIF;
        let untypedInfo = {
          Adverb.adverb = adverb; adverb_fn = fn;
          axes = None; init = None; fixed_args = [];
          array_args = args;
        }
        in
        rewrite_adverb ?src untypedInfo
      | App (fn, args), _ -> rewrite_app src fn (annotate_values args)
      | _ ->
        let errMsg =
          Printf.sprintf
            "Type specialization for %s not implemented"
            (exp_node_to_str expNode)
        in
        raise (RewriteFailed(errMsg, src))
    )

  let rewrite_phi phiNode =
    let t = Hashtbl.find P.tenv phiNode.phi_id in
    let left = rewrite_value t phiNode.phi_left in
    let right = rewrite_value t phiNode.phi_right in
    {phiNode with phi_left = left; phi_right = right }

  let rewrite_phi_nodes phiNodes = List.map rewrite_phi phiNodes

  let rec rewrite_stmt (stmtNode:UntypedSSA.stmt_node) : TypedSSA.stmt_node list =
    IFDEF DEBUG THEN
      Printf.printf "[RewriteTyped.stmt] %s\n%!"
        (UntypedSSA.PrettyPrinters.stmt_node_to_str stmtNode)
    ENDIF;
    let src = stmtNode.UntypedSSA.stmt_src in
    match stmtNode.UntypedSSA.stmt with
    | UntypedSSA.Set(ids, rhs) ->
      let rhsTypes = List.map (Hashtbl.find P.tenv) ids in
      let typedRhs = rewrite_exp rhsTypes rhs in
      let stmtNode = TypedSSA.wrap_stmt ?src $ TypedSSA.Set(ids, typedRhs) in
      collect_coercions () @ [stmtNode]

    | UntypedSSA.SetIdx (lhs, indices, rhs) ->
      let typedArray = annotate_value lhs in
      let indices : TypedSSA.value_node list =
        List.map (coerce_value Type.int32) indices in
      let rhsT =
        Type.peel ~num_axes:(List.length indices) typedArray.TypedSSA.value_type
      in
      let rhs : TypedSSA.exp_node = rewrite_exp [rhsT] rhs in
      let typedStmtNode =
        TypedSSA.wrap_stmt ?src $ TypedSSA.SetIdx(typedArray, indices, rhs)
      in
      collect_coercions() @ [typedStmtNode]

    | UntypedSSA.If(cond, tBlock, fBlock, phiNodes) ->
      let tBlock = rewrite_block tBlock in
      let fBlock = rewrite_block fBlock in
      let phiNodes = rewrite_phi_nodes phiNodes in
      (* do cond last so its coercions don't get mixed with blocks above *)
      let cond = coerce_value Type.bool cond in
      let typedStmtNode =
        TypedSSA.wrap_stmt ?src $ TypedSSA.If(cond, tBlock, fBlock, phiNodes)
      in
      collect_coercions() @ [typedStmtNode]

    | UntypedSSA.WhileLoop(testBlock, testVal, body, header) ->
      let body = rewrite_block body in
      let testBlock = rewrite_block testBlock in
      let header = rewrite_phi_nodes header in
      (* do testVal last so its coercions don't get mixed with blocks above *)
      let testVal = coerce_value Type.bool testVal in
      let typedStmtNode =
        TypedSSA.wrap_stmt ?src $
          TypedSSA.WhileLoop(testBlock, testVal, body, header)
      in
      collect_coercions() @ [typedStmtNode]

  and rewrite_block (untypedBlock : UntypedSSA.block) : TypedSSA.block =
    let typedBlock = Block.create () in
    let process_stmt (untyped:UntypedSSA.stmt_node) =
      let stmts = rewrite_stmt untyped in
      List.iter (fun s -> Block.add typedBlock s) stmts
    in
    Block.iter_forward process_stmt untypedBlock;
    typedBlock

  and rewrite_fn f =
    let body : TypedSSA.block= rewrite_block f.UntypedSSA.body in
    TypedSSA.mk_fn
      ~name:(FnId.get_original_prefix f.UntypedSSA.fn_id)
      ~tenv:(Hashtbl.fold ID.Map.add P.tenv ID.Map.empty)
      ~input_ids:f.UntypedSSA.input_ids
      ~output_ids:f.UntypedSSA.output_ids
      ~body
end

let rewrite_typed ~specializer ~fn ~signature =
  IFDEF DEBUG THEN
    Printf.printf "Rewriting %s for signature %s. Untyped body:\n%s\n"
      (FnId.to_str fn.UntypedSSA.fn_id)
      (Signature.to_str signature)
      (UntypedSSA.PrettyPrinters.fn_to_str fn)
  ENDIF;
  let module Params = struct
    let tenv = TypeAnalysis.type_analysis ~specializer ~fn ~signature
    let specializer = specializer
  end
  in
  let module Rewriter = Make(Params) in
  Rewriter.rewrite_fn fn
