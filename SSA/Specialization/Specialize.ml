(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Helpers
open SSA_AdverbHelpers
open SSA_Codegen
open Type
open Printf

(* make a fresh function definition whose body is only an untyped prim *)
let untypedPrimFnCache : (Prim.t * int, SSA.fn) Hashtbl.t =
  Hashtbl.create 127

let mk_untyped_prim_fn prim arity : SSA.fn =
  let key = (prim,arity) in
  if Hashtbl.mem untypedPrimFnCache key  then
    Hashtbl.find untypedPrimFnCache key
  else
  let inputs = ID.gen_named_list "input" arity in
  let output = ID.gen_named "output" in
  let bottoms = List.map (fun _ -> Type.BottomT) inputs in
  let inputVars = List.map SSA_Helpers.var inputs in
  let rhs = SSA_Helpers.app (SSA_Helpers.wrap_value (Prim prim)) inputVars in
  let body = Block.singleton (SSA_Helpers.set [output] rhs) in
  let fn = SSA_Helpers.mk_fn inputs [output] body in
  (Hashtbl.add untypedPrimFnCache key fn; fn)

let mk_typed_scalar_prim (op : Prim.scalar_op) ?optOutType argTypes =
  let reqArgTypes = TypeInfer.required_scalar_op_types op argTypes in
  let inferredOutType = TypeInfer.infer_scalar_op op reqArgTypes in
  let outType = match optOutType with
    | Some t -> t
    | None -> inferredOutType
  in
  SSA_Codegen.mk_codegen_fn argTypes [outType] $ fun codegen inputs outputs ->
    let args = Array.of_list inputs in
    let inTyArr = Array.of_list argTypes in
    let reqTyArr = Array.of_list reqArgTypes in
    for i = 0 to Array.length args - 1 do
      let reqT = reqTyArr.(i) in
      if inTyArr.(i) <> reqT then begin
        let id = codegen#fresh_var reqT in
        codegen#emit [SSA_Helpers.set [id]  (SSA_Helpers.cast reqT args.(i))];
        args.(i) <- codegen#id_value_node id
      end
    done
    ;
    let primAppNode =
      SSA_Helpers.primapp (Prim.ScalarOp op) [outType] (Array.to_list args)
    in
    let outputVar = List.hd outputs in
    codegen#emit [[outputVar] <-- primAppNode]



(* 1) some statements (ie, those involving array ops)
      make a function definitely not a scalar-only function.
      In a three-value-logic, these give the whole function a value of No.
   2) Other statements have a neutral effect (ie, setting constants).
      These have a value Maybe.
*)



(* checks whether a statement uses an untyped scalar operator *)
let rec is_scalar_stmt stmtNode = match stmtNode.stmt with
  | SSA.Set(_, {exp=SSA.App({value=SSA.Prim (Prim.ScalarOp _)}, _)}) ->
    ThreeValuedLogic.Yes
  | SSA.Set(_, {exp=Values _}) -> ThreeValuedLogic.Maybe
  | SSA.If(_, tCode, fCode, _) ->
      ThreeValuedLogic.combine (is_scalar_block tCode) (is_scalar_block fCode)
  | _ -> ThreeValuedLogic.No

and is_scalar_block block =
  Block.fold_forward
    (fun acc stmtNode -> ThreeValuedLogic.combine acc (is_scalar_stmt stmtNode))
    ThreeValuedLogic.Maybe
    block




let rec specialize_fn fn signature =
  IFDEF DEBUG THEN
    Printf.printf
      "[Specialize.specialize_fn] %s :: %s\n"
      (FnId.to_str fn.fn_id)
      (Signature.to_str signature)
  ENDIF;
  (* first check to see whether we're mapping a function of scalar operators*)
  (* over vector data. if so, rather than creating a large number of Map nodes *)
  (* and then merging them we directly create a single Map. *)
  (* NOTE: scalarizing the function is only done if all the inputs are either
     vectors of the same rank or scalars.
   *)
  let inTypes = Signature.input_types signature in
  let ranks = List.map Type.rank inTypes in
  let maxRank = List.fold_left max 0 ranks in

  if not (Signature.has_output_types signature) &&
     maxRank > 0 &&
     List.for_all (fun r -> r = 0 || r = maxRank) ranks &&
     (is_scalar_block fn.body = ThreeValuedLogic.Yes)
  then scalarize_fn fn signature
  else
  (* to avoid having to make TypeAnalysis and Specialize recursive
       modules I've untied the recursion by making specialize_value
       a parameter of TypeAnalysis.
   *)
  let tenv =
    TypeAnalysis.type_analysis ~specializer:specialize_value ~fn ~signature
  in
  let typedFn =
    RewriteTyped.rewrite_typed ~tenv ~specializer:specialize_value ~fn:fn
  in
  typedFn

and scalarize_fn untyped vecSig =
  IFDEF DEBUG THEN
    Printf.printf "[Specialize.scalarize_fn] %s :: %s\n"
      (FnId.to_str untyped.fn_id)
      (Signature.to_str vecSig);
  ENDIF;
  let inTypes = Signature.input_types vecSig in
  let numAxes = SSA_AdverbHelpers.max_num_axes_from_array_types inTypes in
  let scalarTypes = List.map (Type.peel ~num_axes:numAxes) inTypes in
  IFDEF DEBUG THEN
    Printf.printf
      "[Specialize.scalar_fn] num_axes = %d, scalar_types = %s\n"
      numAxes
      (Type.type_list_to_str scalarTypes)
  ENDIF;
  IFDEF DEBUG THEN
    if not (List.for_all Type.is_scalar scalarTypes) then
      failwith $ Printf.sprintf
        "Expected all inputs to be scalars, got %s"
        (Type.type_list_to_str scalarTypes)
  ENDIF;
  let scalarSig = Signature.from_input_types scalarTypes in
  let scalarFn = specialize_value (SSA.GlobalFn untyped.fn_id) scalarSig in
  let scalarOutputTypes = scalarFn.fn_output_types in
  let outTypes = List.map (Type.increase_rank 1) scalarOutputTypes in
  let scalarClosure = SSA_Helpers.closure scalarFn [] in
  SSA_Codegen.mk_codegen_fn inTypes outTypes (fun codegen inputs outputs ->
    let outIds = List.map SSA_Helpers.get_id outputs in
    codegen#emit [
      SSA_Helpers.set outIds (mk_map scalarClosure inputs)
    ]
  )

and specialize_value fnVal signature =
  IFDEF DEBUG THEN
    Printf.printf "[Specialize.specialize_value] %s :: %s\n%!"
      (SSA.value_to_str fnVal)
      (Signature.to_str signature)
    ;
  ENDIF;
  match FnManager.maybe_get_specialization fnVal signature with
  | Some fnId -> FnManager.get_typed_function fnId
  | None ->
    let inputTypes = Signature.input_types signature in
    (match fnVal with
      | SSA.GlobalFn fnId ->
        let untyped = FnManager.get_untyped_function fnId in
        let typed = specialize_fn untyped signature in
        FnManager.add_specialization ~optimize:true fnVal signature typed;
        typed
      | SSA.Prim (Prim.ScalarOp op) ->
          let optOutType =
            Option.map List.hd (Signature.output_types_option signature)
          in
          let typed =
            if List.for_all Type.is_scalar inputTypes
            then mk_typed_scalar_prim op ?optOutType inputTypes
            else
            (* if we're adding two arrays, then turn it into a map over both *)
            (* axes, but if we're adding an array to a vector we can only map*)
            (* over one axis *)
              let maxRank =
                SSA_AdverbHelpers.max_num_axes_from_array_types inputTypes
              in
              let nestedInputTypes =
                List.map (Type.peel ~num_axes:maxRank) inputTypes
              in
              let nestedSig = match optOutType with
                | None ->
                  Signature.from_input_types nestedInputTypes
                | Some outT ->
                  let nestedOutTypes = [Type.peel ~num_axes:maxRank outT] in
                  Signature.from_types nestedInputTypes nestedOutTypes
              in
              let nestedFn = specialize_value fnVal nestedSig in
              SSA_AdverbHelpers.mk_map_fn
                ?src:None
                ~nested_fn:nestedFn
                ~axes:(SSA_AdverbHelpers.infer_adverb_axes_from_rank maxRank)
                ~fixed_types:[]
                ~array_types:inputTypes
          in
          FnManager.add_specialization ~optimize:false fnVal signature typed;
          typed

      | SSA.Prim p ->
          let arity = List.length inputTypes in
          assert (arity >= Prim.min_prim_arity p &&
                  arity <= Prim.max_prim_arity p);
          let untyped = mk_untyped_prim_fn p arity in
          let typed = specialize_fn untyped signature in
          FnManager.add_specialization ~optimize:false  fnVal signature typed;
          typed
      | _ -> assert false
    )

let specialize_fn_id fnId signature = specialize_value (GlobalFn fnId) signature
