(* pp: -parser o pa_macro.cmo *)
open Base
open SSA
open SSA_Helpers
open SSA_Transform
open Printf
open Type

module type REWRITE_PARAMS = sig
  val output_arity : value -> int
  val specializer : value -> Signature.t -> fn
  val closure_env : CollectPartialApps.closure_env
  val tenv : (ID.t, Type.t) Hashtbl.t
end

module Rewrite_Rules (P: REWRITE_PARAMS) = struct


  let get_type id = Hashtbl.find P.tenv id
  let set_type id t = Hashtbl.replace P.tenv id t
  let fresh_id t =
    let id = ID.gen() in
    set_type id t;
    id

  let is_closure id = Hashtbl.mem P.closure_env.CollectPartialApps.closures id
  let get_closure_val id =
    Hashtbl.find P.closure_env.CollectPartialApps.closures id
  let get_closure_args id =
    Hashtbl.find P.closure_env.CollectPartialApps.closure_args id
  let get_closure_arity id =
    Hashtbl.find P.closure_env.CollectPartialApps.closure_arity id

  let infer_value_type = function
    | Var id -> if is_closure id then Type.BottomT else get_type id
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
    | Var id ->
      let closureArgs = get_closure_args id in
      let closureArgTypes = List.map infer_value_node_type closureArgs in
      let signature' =
        Signature.prepend_input_types closureArgTypes signature
      in
      let fnVal' = get_closure_val id in
      let fundef = P.specializer fnVal' signature' in
      {
        closure_fn = fundef.fn_id;
        closure_args = closureArgs;
        closure_arg_types = closureArgTypes;
        closure_input_types =
          keep_tail closureArgTypes fundef.fn_input_types;
        closure_output_types = fundef.fn_output_types;
      }
    | GlobalFn _
    | Prim _ -> mk_closure (P.specializer fnVal signature) []
    | _ -> assert false

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
        mk_num ?src:valNode.value_src ~ty:t n'
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
        let coerceExp = mk_cast t valNode in
        let id' =  fresh_id t in
        add_coercion (mk_set [id'] coerceExp);
        mk_var ~ty:t id'
    | _ -> rewrite_value t valNode

  let coerce_values t vs = List.map (coerce_value t) vs

  let rewrite_adverb src adverb fnVal argNodes argTypes =
    match adverb with
      | Prim.Map ->
        let eltTypes = List.map Type.peel argTypes in
        let closure =
          mk_typed_closure fnVal (Signature.from_input_types eltTypes)
        in
        mk_map closure argNodes
      | Prim.Reduce ->
        let arity = P.output_arity fnVal in
        let initArgs, args = List.split_nth arity argNodes in
        let initTypes, argTypes = List.split_nth arity argTypes in
        let eltTypes = List.map Type.peel argTypes in
        (* TODO: fix this nonsense *)
        let accTypes = [Type.AnyT] in
        let reduceSignature =
          Signature.from_types (accTypes @ eltTypes) accTypes
        in
        let reduceClosure = mk_typed_closure fnVal reduceSignature in
        mk_reduce ?src ~axes:[0] reduceClosure initArgs args
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
        let whereExp = mk_primapp ?src whereOp ~output_types:[whereT] [index] in
        add_coercion (mk_set ?src [whereId] whereExp);
        let args' = array :: [mk_var ?src ~ty:whereT whereId] in
        let resultType = Type.ArrayT (Type.elt_type arrayType, 1) in
        let indexOp = Prim.ArrayOp Prim.Index in
        mk_primapp ?src indexOp ~output_types:[resultType] args'
    | _ ->
        let outT = TypeInfer.infer_simple_array_op op types in
        mk_primapp ?src (Prim.ArrayOp op) ~output_types:[outT] args

  let rewrite_index src lhs args =
    let arrType = infer_value_node_type lhs in
    (* TODO: fix *)
    let outTypes = [Type.AnyT] in
    let arrNode = {lhs with value_type = arrType} in
    let indexOp = Prim.ArrayOp Prim.Index in
    mk_primapp ?src indexOp  ~output_types:outTypes (arrNode::args)

  let rewrite_call src varId args argTypes =
    let fnVal = get_closure_val varId in
    let closureArgs = get_closure_args varId in
    let closureArgTypes = List.map infer_value_node_type closureArgs in
    let types = closureArgTypes @ argTypes in
    let typedFn = P.specializer fnVal (Signature.from_input_types types) in
    mk_call ?src typedFn.fn_id typedFn.fn_output_types (closureArgs @ args)


  let rewrite_app src fn argNodes : exp_node =
    let argTypes = List.map (fun v -> v.value_type) argNodes in
    let fnVal = fn.value in
    match fnVal with
    | Prim ((Prim.ScalarOp op) as p) ->
      let outT = TypeInfer.infer_scalar_op op argTypes in
      if Type.is_array outT then
        rewrite_adverb src Prim.Map fnVal argNodes argTypes
      else
        (* most scalar ops expect all their arguments to be of the same*)
        (* type, except for Select, whose first argument is always a bool *)
        let sameTypeArgNodes : SSA.value_node list =
          match op, argNodes, argTypes with
          | Prim.Select, boolArg::otherArgs, _::otherTypes ->
            let inT = Type.combine_type_list otherTypes in
            boolArg :: (List.map (coerce_value inT) argNodes)
          | _ ->
            (* if operation is a float, then make sure the inputs are*)
            (* at least float32 *)
            let inT =
              if Prim.is_float_unop op then
                Type.combine_type_list (Type.float32::argTypes)
              else
                Type.combine_type_list argTypes
            in
            List.map (coerce_value inT) argNodes
        in
        SSA_Helpers.mk_primapp p [outT] sameTypeArgNodes

    | Prim (Prim.ArrayOp op) -> rewrite_array_op src op argNodes argTypes
    | Prim (Prim.Adverb adverb) ->
      begin match argNodes, argTypes with
        | fn::args, _::argTypes ->
          rewrite_adverb src adverb fn.value args argTypes
        | _ -> assert false
      end
    | GlobalFn _ ->
      let typed = P.specializer fnVal (Signature.from_input_types argTypes) in
      mk_call ?src typed.fn_id typed.fn_output_types argNodes
    | Var id ->
      if is_closure id then rewrite_call src id  argNodes argTypes
      else rewrite_index src fn argNodes
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

    | SetIdx (arrayId, indices, rhs) -> failwith "setidx not implemented"

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
    let tenv = Hashtbl.fold ID.Map.add P.tenv ID.Map.empty in
    mk_fn ~tenv ~input_ids:f.input_ids ~output_ids:f.output_ids ~body
end

let rewrite_typed ~tenv ~closureEnv ~specializer ~output_arity ~fn =
  let module Params = struct
    let tenv = tenv
    let closure_env = closureEnv
    let specializer = specializer
    let output_arity = output_arity
  end
  in
  let module Transform = Rewrite_Rules(Params) in
  Transform.transform_fn fn
