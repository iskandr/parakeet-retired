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
      | UntypedSSA.NoneVal -> 
        {
          TypedSSA.value = TypedSSA.NoneVal; 
          value_type = Type.NoneT;
          value_src = src;  
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

  let add_coercion ?src ?(name="coerce") expNode =
    let t = List.hd expNode.TypedSSA.exp_types in
    let id = fresh_id ~name t in
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

  let specialize_fn 
        ?src 
        adverb 
        untypedFnVal 
        fixedTypes 
        initTypes
        eltTypes = 
    let nestedSig : Signature.t =
      match adverb, initTypes with

      | Adverb.Map, None  ->
        Signature.from_input_types (fixedTypes @ eltTypes)
      | Adverb.Reduce, None ->
        if List.length eltTypes <> 1 then
          failwith "Reduce without initial args can only have 1 input"
        else
        let eltT = List.hd eltTypes in
        let inputArity : int = untyped_value_input_arity untypedFnVal in
        if inputArity <> 2 then
          let errMsg : string =
            Printf.sprintf
              "Reduce without init requires binary operator, given %s (arity %d)"
              (UntypedSSA.PrettyPrinters.value_to_str untypedFnVal)
              inputArity
          in
          raise $ RewriteFailed(errMsg, src)
        else
        (* assume that the accumulator is the same as the array element type *)
        Signature.from_input_types (fixedTypes @ [eltT; eltT])
      | Adverb.Reduce, Some initTypes -> 
        Signature.from_input_types (fixedTypes @ initTypes @ eltTypes) 
      | Adverb.Map, Some _ -> failwith "Map can't have initial args"
     
      | _ -> failwith "Malformed adverb"
    in
    P.specializer untypedFnVal nestedSig 
  
  let rewrite_adverb_for_typed_args
        ?(src:SrcInfo.t option)
        (info :
           (UntypedSSA.value_node, TypedSSA.value_nodes, TypedSSA.value_nodes)
           Adverb.t) : TypedSSA.exp_node =
    let arrayTypes = TypedSSA.types_of_value_nodes info.args in
    let fixedTypes = TypedSSA.types_of_value_nodes info.fixed in
    let numAxes = List.length info.axes in
    let eltTypes = List.map (Type.peel ~num_axes:numAxes) arrayTypes in
    let initTypes = Option.map TypedSSA.types_of_value_nodes info.init in  
    let untypedFnVal = info.fn.UntypedSSA.value  in
    let typedFn : TypedSSA.fn  = 
      specialize_fn 
        info.adverb_type
        untypedFnVal 
        fixedTypes 
        initTypes 
        eltTypes 
    in 
    let outTypes : Type.t list =
      TypeAnalysis.infer_adverb_result_types
        ~adverb_type:info.adverb_type
        ~elt_result_types:(typedFn.TypedSSA.fn_output_types)
        ~num_axes:(List.length info.axes)
    in
    (* change any init values to have same type as final return *)
    let typedInfo  = 
      match info.init with 
      | None -> 
          { info with 
              fn = TypedSSA.fn_id typedFn;
              combine = None; (*Option.map TypedSSA.fn_id typedCombine;*) 
          } 
      | Some initVals -> 
        assert (List.length initVals = List.length outTypes);
        IFDEF DEBUG THEN 
          Printf.printf 
            "RE-SPECIALIZING FUNCTION %s WITH FIXED (%s) INIT (%s) ELT (%s)\n%!"
            (UntypedSSA.value_to_str untypedFnVal)
            (Type.type_list_to_str fixedTypes)
            (Type.type_list_to_str outTypes)
            (Type.type_list_to_str eltTypes)
       ENDIF;  
        let typedFn = 
          specialize_fn 
            info.adverb_type
            untypedFnVal
            fixedTypes
            (Some outTypes)
            eltTypes
        in 
        { info with 
            fn = TypedSSA.fn_id typedFn;
            combine = None; 
            init = Some (List.map2 coerce_typed_value outTypes initVals); 
        } 
    in  
    { TypedSSA.exp = TypedSSA.Adverb typedInfo;
      exp_types = outTypes;
      exp_src = None;
    }

  let rewrite_adverb
        ?(src:SrcInfo.t option)
        (info : UntypedSSA.adverb_info) : TypedSSA.exp_node =
    (* make typed version of all the adverbinfo fields *)
    let arrayArgs : TypedSSA.value_nodes = annotate_values info.args in
    let arrayTypes = TypedSSA.types_of_value_nodes arrayArgs in
    let axes : TypedSSA.value_nodes = match Option.map annotate_values info.axes with
      | None  
      | Some [{TypedSSA.value_type = Type.NoneT}] ->  
        AdverbHelpers.infer_adverb_axes_from_types arrayTypes
      | Some axes -> axes
    in
    let partiallyTypedInfo = {
      info with
        fixed = annotate_values info.fixed;
        args = arrayArgs;
        axes = axes;
        init = Option.map annotate_values info.init;
    }
    in
    rewrite_adverb_for_typed_args ?src partiallyTypedInfo




  (* map a function which does element-by-element indexing over an *)
  (* array of indices *)
  let map_index_operator ?src array indices =
    let untypedIndexNode = {
      UntypedSSA.value = UntypedSSA.Prim (Prim.ArrayOp Prim.Index);
      value_src = src;
    }
    in
    rewrite_adverb_for_typed_args ?src {
      Adverb.adverb_type = Adverb.Map;
      fn = untypedIndexNode;
      combine = None; 
      fixed = [array];
      init = None;
      axes = List.map TypedSSA.int32 $ List.til (List.length indices);
      args = indices;
    }

  let rewrite_call_to_array_op
        (src: SrcInfo.t option)
        (op:Prim.array_op)
        (args:TypedSSA.value_nodes)
        (types:Type.t list) : TypedSSA.exp_node =
    match op, args, types with
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
      let whereResult = add_coercion ?src ~name:"where_result" whereExp in
      map_index_operator array [whereResult]
    | Prim.Index, array::indices, arrayT::indexTypes
        when List.exists Type.is_array indexTypes ->
      map_index_operator ?src array indices
    | _ ->
        let outT = TypeAnalysis.infer_simple_array_op op types in
        TypedSSA.primapp ?src (Prim.ArrayOp op) ~output_types:[outT] args
        
        
  let rewrite_call_to_scalar_op 
      (op : Prim.scalar_op) 
      (args :  TypedSSA.value_node list) 
      (argTypes :  Type.t list) = 
    let p = Prim.ScalarOp op in 
    let outT = TypeAnalysis.infer_scalar_op op argTypes in
    if Type.is_array outT then
      let axes =
        AdverbHelpers.infer_adverb_axes_from_types argTypes
      in
      let eltTypes =
        List.map (fun t -> Type.ScalarT (Type.elt_type t)) argTypes
      in
      let typedFn =
        P.specializer 
          (UntypedSSA.Prim p)
          (Signature.from_input_types eltTypes)
      in
      let typedInfo : TypedSSA.adverb_info =
        {
          Adverb.adverb_type = Adverb.Map;
          fn = typedFn.TypedSSA.fn_id;
          combine = None; 
          fixed = [];
          init = None;
          args = args;
          axes = axes;
        }
      in
      AdverbHelpers.mk_adverb_exp_node typedInfo

    else
      (* most scalar ops expect all their arguments to be of the same*)
      (* type, except for Select, whose first argument is always a bool *)
      begin
        match op, args, argTypes with
        | Prim.Select, boolArg::otherArgs, _::otherTypes ->
          let inT = Type.combine_type_list otherTypes in
          let args =
            (coerce_typed_value Type.bool boolArg)
            ::
            (coerce_typed_values inT args)
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
          let args = coerce_typed_values inT args in
          TypedSSA.primapp p [outT] args
      end

  let rewrite_call src
      (fn:UntypedSSA.value_node)
      (args : TypedSSA.value_node Args.actual_args) : TypedSSA.exp_node =
    let argTypes = 
      Args.apply_to_actual_values (fun v -> v.TypedSSA.value_type) args 
    in
    let fnVal = fn.UntypedSSA.value in
    let fnSrc = fn.UntypedSSA.value_src in
    match fnVal with
      | UntypedSSA.Prim (Prim.ScalarOp op) ->
        rewrite_call_to_scalar_op op args.Args.values argTypes.Args.values 
      | UntypedSSA.Prim (Prim.ArrayOp op) ->
        rewrite_call_to_array_op src op args.Args.values argTypes.Args.values
      | UntypedSSA.Prim (Prim.Adverb adverb) ->
        raise (RewriteFailed(
        "Unexpected adverb, should have been handled in rewrite_exp", fnSrc))
      
      | UntypedSSA.GlobalFn untypedFnId ->
        let typedFn = P.specializer fnVal (Signature.from_args argTypes) in
        let outputTypes = TypedSSA.output_types typedFn in
        let untypedFormals : UntypedSSA.value_node Args.formal_args = 
          FnManager.get_untyped_args untypedFnId 
        in 
        let typedFormals : TypedSSA.value_node Args.formal_args = 
          Args.apply_to_formal_values annotate_value untypedFormals 
        in 
        let withTypedDefaults = List.map snd (Args.bind typedFormals args) in 
        TypedSSA.call ?src typedFn.TypedSSA.fn_id outputTypes withTypedDefaults
      | UntypedSSA.Var id ->
        (* assume variable must be an array *)
        let arrType = Hashtbl.find P.tenv id in
        let indices = args.Args.values in 
        let outTypes = [Type.peel ~num_axes:(List.length indices) arrType] in
        let arrNode = TypedSSA.var ?src:fnSrc arrType id in
        let indexOp = Prim.ArrayOp Prim.Index in
        TypedSSA.primapp 
          ?src indexOp  ~output_types:outTypes (arrNode::indices)

      | _ ->
        let errMsg =
          Printf.sprintf "[RewriteTyped] Unexpected function: %s"
            (UntypedSSA.value_node_to_str fn)
        in
        raise (RewriteFailed(errMsg, fnSrc))

  let rewrite_exp types expNode =
    (*IFDEF DEBUG THEN
      Printf.printf "[RewriteTyped.exp] %s :: %s\n%!"
        (UntypedSSA.PrettyPrinters.exp_node_to_str expNode)
        (Type.type_list_to_str types)
    ENDIF;*)
    UntypedSSA.(
      let src = expNode.exp_src in
      match expNode.exp, types with
      | Array elts, [Type.ArrayT(eltT, _)] ->
        let elts' = coerce_values (Type.ScalarT eltT) elts in
        { TypedSSA.exp = TypedSSA.Arr elts'; exp_types = types; exp_src = src}
      | Values vs, _ ->
        let vs' = List.map2 coerce_value types vs in
        { TypedSSA.exp = TypedSSA.Values vs'; exp_types = types; exp_src = src }
        (* WARNING: You're ignoring the expected return types here *)
      | Adverb adverbInfo, _ -> rewrite_adverb ?src adverbInfo
      (*
      | Call (
              {value = Prim (Prim.Adverb adverb)}, 
              {Args.values=fn::args; keywords=[]}
             ), _ ->
        IFDEF DEBUG THEN
          Printf.printf "[RewriteTyped] Converting simple adverb\n%!";
        ENDIF;
        let untypedInfo = {
          Adverb.adverb_type = adverb_type; fn = fn;
          axes = None; init = None; fixed = [];
          args = args;
        }
        in
        rewrite_adverb ?src untypedInfo
      *) 
      | Call (fn, args), _ -> 
        let typedArgs = Args.apply_to_actual_values annotate_value args in
        rewrite_call src fn typedArgs
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
    (*IFDEF DEBUG THEN
      Printf.printf "[RewriteTyped.stmt] %s\n%!"
        (UntypedSSA.PrettyPrinters.stmt_node_to_str stmtNode)
    ENDIF;*)
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

  and rewrite_fn (fn:UntypedSSA.fn) =
    let body : TypedSSA.block = rewrite_block fn.UntypedSSA.body in
    let tenv = Hashtbl.fold ID.Map.add P.tenv ID.Map.empty in 
    let formals = fn.UntypedSSA.inputs in
    let formalNames = Args.all_formal_names formals in  
    let get_input_id name = 
      String.Map.find name fn.UntypedSSA.input_names_to_ids 
    in 
    let formalIds = List.map get_input_id formalNames in   
    TypedSSA.mk_fn
      ~name:(FnId.get_original_prefix fn.UntypedSSA.fn_id)
      ~tenv:tenv
      ~input_ids:formalIds
      ~output_ids:fn.UntypedSSA.output_ids
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
