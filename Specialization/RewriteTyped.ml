(* pp: -parser o pa_macro.cmo *)
open Base
open SSA
open SSA_AdverbHelpers
open SSA_Transform
open Printf
open Type

module type REWRITE_PARAMS = sig
  val specializer : value -> Signature.t -> fn
  val tenv : (ID.t, Type.t) Hashtbl.t
end

module Rewrite_Rules (P: REWRITE_PARAMS) = struct



  let get_type id =
    match Hashtbl.find_option P.tenv id with
      | Some t -> t
      | None -> failwith $ "No type for " ^ (ID.to_str id)


  let set_type id t = Hashtbl.replace P.tenv id t
  let fresh_id t =
    let id = ID.gen_named "temp" in
    set_type id t;
    id

  let infer_value_type = function
    | Var id -> get_type id
    | Num n -> Type.ScalarT (ParNum.type_of n)
    | other -> Type.BottomT

  let infer_value_node_type valNode =
    infer_value_type valNode.value

  (* keeps only the portion of the second list which is longer than the first *)
  let rec keep_tail l1 l2 =
    if l1 = [] then l2
    else if l2 = [] then []
    else keep_tail (List.tl l1) (List.tl l2)

  let mk_typed_closure fnVal signature =
    match fnVal with
    | GlobalFn _
    | Prim _ -> SSA_Helpers.closure (P.specializer fnVal signature) []
    | _ ->
      failwith $ Printf.sprintf
        "[RewriteTyped] Expected function, got variable %s "
        (SSA.value_to_str fnVal)

  let annotate_value valNode =
    let t = infer_value_node_type valNode in
    if t <> valNode.value_type then { valNode with value_type = t}
    else valNode

  let annotate_values vs = List.map annotate_value vs

  (* rewrite a value to have type t, but can't create coercion statements
     for variables of the wrong type
   *)
  let rewrite_value (t:Type.t) (valNode:SSA.value_node) =
    if valNode.value_type = t then valNode
    else match valNode.value with
      | Num n ->
        let n' = ParNum.coerce n (Type.elt_type t) in
        SSA_Helpers.num ?src:valNode.value_src ~ty:t n'
      | Var id ->
        let t' = get_type id in
        if t = t' then {valNode with value_type = t }
        else failwith $
          Printf.sprintf
            "Cannot rewrite %s : %s to become %s"
            (ID.to_str id)
            (Type.to_str t')
            (Type.to_str t)
      | _ ->
        failwith  $ Printf.sprintf "Can't coerce value %s to type %s"
          (value_node_to_str valNode) (Type.to_str t)


  (* if a value needs to be coerced, then it gets added to this list.
     It's the job of any caller to check and clear this list
   *)
  let coercions = ref []

  let add_coercion stmtNode =
    coercions := stmtNode :: !coercions

  let get_coercions () = !coercions

  let clear_coercions () = coercions := []

  let collect_coercions stmtNode =
    let stmts = stmtNode :: get_coercions()  in
    clear_coercions ();
    stmts

  let coerce_value (t:Type.t) (valNode:SSA.value_node) =
    if valNode.value_type = t then valNode
    else match valNode.value with
    | Var id ->
        let t' = get_type id in
        if t = t' then {valNode with value_type = t }
        else
        let coerceExp = SSA_Helpers.cast t valNode in
        let id' =  fresh_id t in
        add_coercion (SSA_Helpers.set [id'] coerceExp);
        SSA_Helpers.var ~ty:t id'
    | _ -> rewrite_value t valNode

  let coerce_values t vs = List.map (coerce_value t) vs

  let rewrite_adverb src adverb fnVal
        ?(closure_args=[])
        ?arg_types
        ?init
        ?axes
        argNodes  =
    IFDEF DEBUG THEN
      Printf.printf
        "[RewriteTyped.rewrite_adverb] %s(fn=%s, args=[%s])\n"
        (Prim.adverb_to_str adverb)
        (SSA.value_to_str fnVal)
        (SSA.value_nodes_to_str argNodes)
      ;
    ENDIF;
    assert (init=None);
    assert (axes=None);
    assert (closure_args=[]);
    let argTypes = match arg_types with
      | None -> SSA_Helpers.types_of_value_nodes argNodes
      | Some types -> types
    in
    let axes = SSA_AdverbHelpers.infer_adverb_axes_from_types ?axes argTypes in
    let numAxes = List.length axes in
    match adverb with
      | Prim.Map ->
        let eltTypes = List.map (Type.peel ~num_axes:numAxes) argTypes in
        let closure =
          mk_typed_closure fnVal (Signature.from_input_types eltTypes)
        in
        SSA_AdverbHelpers.mk_map closure argNodes
      | Prim.Reduce ->
        let arity = FnManager.output_arity_of_value fnVal in
        let initArgs, args = List.split_nth arity argNodes in
        let initTypes, argTypes = List.split_nth arity argTypes in
        let eltTypes = List.map Type.peel argTypes in
        (* TODO: fix this nonsense *)
        let accTypes = [Type.AnyT] in
        let reduceSignature =
          Signature.from_types (accTypes @ eltTypes) accTypes
        in
        let reduceClosure = mk_typed_closure fnVal reduceSignature in
        SSA_AdverbHelpers.mk_reduce ?src ?axes:None reduceClosure initArgs args
      | Prim.AllPairs ->
        (*let eltTypes = List.map Type.peel_vec argTypes in
        let eltSignature = Signature.from_input_types eltTypes in
        *)
        assert false
      | other -> failwith $ (Prim.adverb_to_str other) ^ " not implemented"


  let rewrite_array_op
        (src: SrcInfo.t option)
        (op:Prim.array_op)
        (args:SSA.value_node list)
        (types:Type.t list) = match op, args, types with
    | Prim.Index, [array; index], [arrayType; indexType]
        when Type.elt_type indexType = Type.BoolT ->
        IFDEF DEBUG THEN
            if Type.rank indexType <> 1 then
              failwith "Expected boolean index vector to be 1D"
        ENDIF;
        let whereT = Type.ArrayT(Type.Int32T, 1) in
        let whereId = fresh_id whereT in
        let whereOp =  Prim.ArrayOp Prim.Where in
        let whereExp =
          SSA_Helpers.primapp ?src whereOp ~output_types:[whereT] [index]
        in
        add_coercion (SSA_Helpers.set ?src [whereId] whereExp);
        let args' = array :: [SSA_Helpers.var ?src ~ty:whereT whereId] in
        let resultType = Type.ArrayT (Type.elt_type arrayType, 1) in
        let indexOp = Prim.ArrayOp Prim.Index in
        SSA_Helpers.primapp ?src indexOp ~output_types:[resultType] args'
    | _ ->
        let outT = TypeInfer.infer_simple_array_op op types in
        SSA_Helpers.primapp ?src (Prim.ArrayOp op) ~output_types:[outT] args

  let rewrite_index src lhs args =
    let arrType = infer_value_node_type lhs in
    let outTypes = [Type.AnyT] in
    let arrNode = {lhs with value_type = arrType} in
    let indexOp = Prim.ArrayOp Prim.Index in
    SSA_Helpers.primapp ?src indexOp  ~output_types:outTypes (arrNode::args)

  let rewrite_app src fn argNodes : exp_node =
    IFDEF DEBUG THEN
      Printf.printf "[RewriteTyped.rewrite_app] %s(%s)\n"
        (SSA.value_node_to_str fn)
        (SSA.value_nodes_to_str argNodes)
      ;
    ENDIF;
    let argTypes = List.map (fun v -> v.value_type) argNodes in
    let fnVal = fn.value in
    match fnVal with
    | Prim ((Prim.ScalarOp op) as p) ->
      let outT = TypeInfer.infer_scalar_op op argTypes in
      if Type.is_array outT then
        rewrite_adverb src Prim.Map fnVal ~arg_types:argTypes argNodes
      else
        (* most scalar ops expect all their arguments to be of the same*)
        (* type, except for Select, whose first argument is always a bool *)
        begin
          match op, argNodes, argTypes with
          | Prim.Select, boolArg::otherArgs, _::otherTypes ->
            let inT = Type.combine_type_list otherTypes in
            let args = boolArg :: (List.map (coerce_value inT) argNodes) in
            SSA_Helpers.primapp p [outT] args
          | _ ->
            (* if operation is a float, then make sure the inputs are*)
            (* at least float32 *)
            let inT =
              if Prim.is_float_unop op then
                Type.combine_type_list (Type.float32::argTypes)
              else
                Type.combine_type_list argTypes
            in
            let args = List.map (coerce_value inT) argNodes in
            SSA_Helpers.primapp p [outT] args
        end
    | Prim (Prim.ArrayOp op) -> rewrite_array_op src op argNodes argTypes
    | Prim (Prim.Adverb adverb) ->
      begin match argNodes, argTypes with
        | fn::args, _::argTypes ->
          rewrite_adverb src adverb fn.value ~arg_types:argTypes args
        | _ -> assert false
      end
    | GlobalFn _ ->
      let typed = P.specializer fnVal (Signature.from_input_types argTypes) in
      SSA_Helpers.call ?src typed.fn_id typed.fn_output_types argNodes
    | Var id -> rewrite_index src fn argNodes
    | _ -> failwith $
             Printf.sprintf "[RewriteTyped] Unexpected function: %s"
              (SSA.value_node_to_str fn)

  let rewrite_exp types expNode =
    match expNode.exp, types with
      | Arr elts, [Type.ArrayT(eltT, _)] ->
        let elts' = coerce_values (Type.ScalarT eltT) elts in
        {expNode with exp = Arr elts'; exp_types = types}
      | Values vs, _ ->
        let vs' = List.map2 coerce_value types vs in
        {expNode with exp = Values vs'; exp_types = types }
      | App (fn, args), _ ->
        rewrite_app expNode.exp_src fn (annotate_values args)
      | Adverb (adverb, {closure_fn; closure_args}, {init; axes; args}), _ ->
        let args' = annotate_values args in
        rewrite_adverb expNode.exp_src adverb (GlobalFn closure_fn) args'
      | _ -> failwith $
               Printf.sprintf
                 "[RewriteTyped] Type specialization for %s not implemented"
                 (SSA.exp_to_str expNode)

  let rewrite_phi phiNode =
    let t = Hashtbl.find P.tenv phiNode.phi_id in
    let left = rewrite_value t phiNode.phi_left in
    let right = rewrite_value t phiNode.phi_right in
    {phiNode with phi_type = t; phi_left = left; phi_right = right }

  let rewrite_phi_nodes phiNodes = List.map rewrite_phi phiNodes

  let rec stmt stmtNode : stmt_node list =
    match stmtNode.stmt with
    | Set(ids, rhs) ->
      let rhsTypes = List.map (Hashtbl.find P.tenv) ids in
      let rhs' = rewrite_exp rhsTypes rhs in
      let stmtNode' = {stmtNode with stmt = Set(ids, rhs')} in
      collect_coercions stmtNode'

    | SetIdx (lhs, indices, rhs) ->
      let array = annotate_value lhs in
      let indices = List.map (coerce_value Type.int32) indices in
      let rhsT = Type.peel ~num_axes:(List.length indices) array.value_type in
      let rhs = coerce_value rhsT rhs in
      let stmtNode' = { stmtNode with stmt = SetIdx(lhs, indices, rhs) } in
      collect_coercions stmtNode'


    | If(cond, tBlock, fBlock, phiNodes) ->
      let typedCond = annotate_value cond in
      let boolCond = coerce_value Type.bool typedCond in
      let tBlock' = transform_block tBlock in
      let fBlock' = transform_block fBlock in
      let phiNodes' = rewrite_phi_nodes phiNodes in
      let stmtNode' =
        {stmtNode with stmt = If(boolCond, tBlock', fBlock', phiNodes')}
      in
      collect_coercions stmtNode'

    | WhileLoop(testBlock, testVal, body, header) ->
        let body' = transform_block body in
        let testBlock' = transform_block testBlock in
        let typedTestVal = annotate_value testVal in
        let boolTestVal = coerce_value Type.bool typedTestVal in
        let header' = rewrite_phi_nodes header in
        let stmtNode' =  { stmtNode with
            stmt = WhileLoop(testBlock', boolTestVal, body', header')
        }
        in collect_coercions stmtNode'

  and transform_block block =
    let buffer = ref [] in
    let process_stmt stmtNode =
      let newStmts = stmt stmtNode in
      buffer := newStmts @ !buffer
    in
    Block.iter_forward process_stmt block;
    Block.of_list (List.rev !buffer)

  and transform_fn f =
    let body = transform_block f.body in
    SSA_Helpers.mk_fn
      ~name:(FnId.get_original_prefix f.fn_id)
      ~tenv:(Hashtbl.fold ID.Map.add P.tenv ID.Map.empty)
      ~input_ids:f.input_ids
      ~output_ids:f.output_ids
      ~body
end

let rewrite_typed ~tenv ~specializer ~fn =
  let module Params = struct
    let tenv = tenv
    let specializer = specializer

  end
  in
  let module Transform = Rewrite_Rules(Params) in
  Transform.transform_fn fn