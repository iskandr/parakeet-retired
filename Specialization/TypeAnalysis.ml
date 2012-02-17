(* pp: -parser o pa_macro.cmo *)
open Printf
open Base
open SSA
open SSA_Analysis

module type TYPE_ANALYSIS_PARAMS = sig
  val infer_output_types : value -> Signature.t -> Type.t list
  val signature : Signature.t
end


exception TypeError of string * (SrcInfo.t option)

let get_type_or_bottom tenv id = Hashtbl.find_default tenv id Type.BottomT
let get_type tenv id = match Hashtbl.find_option tenv id with
  | Some t -> t
  | None -> failwith $ Printf.sprintf "Couldn't find type for variable %s"
      (ID.to_str id)
let add_type tenv id t = Hashtbl.add tenv id t; tenv

(* TODO: make this complete for all SSA statements *)
let rec is_scalar_stmt = function
  | SSA.Set(_, {exp=SSA.App({value=SSA.Prim (Prim.ScalarOp _)}, _)})
  | SSA.Set(_, {exp=Values _}) -> true
  | SSA.If(_, tCode, fCode, _) ->
      all_scalar_stmts tCode && all_scalar_stmts fCode
  | _ -> false
and is_scalar_stmt_node stmtNode = is_scalar_stmt stmtNode.stmt
and all_scalar_stmts stmts = Block.for_all is_scalar_stmt_node stmts

module MkAnalysis (P : TYPE_ANALYSIS_PARAMS) = struct
  let iterative = true
  let dir = Forward

  type env = (ID.t, Type.t) Hashtbl.t
  type exp_info = Type.t list
  type value_info = Type.t

  let init fundef =
    let tenv = Hashtbl.create 127 in
    let inputIds = ref fundef.input_ids in
    let inputTypes = ref (Signature.input_types P.signature) in
    IFDEF DEBUG THEN
      if List.length !inputIds <> List.length !inputTypes then
        failwith  $ Printf.sprintf
            "[TypeAnalysis]
                mismatching number of input IDs (%s) and types (%s) in %s"
            (String.concat ", " (List.map ID.to_str !inputIds))
            (Type.type_list_to_str !inputTypes)
            (FnId.to_str fundef.fn_id)
    ENDIF;
    while !inputIds <> [] && !inputTypes <> [] do
      Hashtbl.add tenv (List.hd !inputIds) (List.hd !inputTypes);
      inputIds := List.tl !inputIds;
      inputTypes := List.tl !inputTypes
    done;
    if Signature.has_output_types P.signature then (
      let outputIds = ref fundef.output_ids in
      let outputTypes = ref (Signature.output_types P.signature) in
      IFDEF DEBUG THEN
        if List.length !outputIds <> List.length !outputTypes then
          let msg =
            Printf.sprintf
              "mismatching number of output IDs (%s) and types (%s) in %s"
              (String.concat ", " (List.map ID.to_str !outputIds))
              (Type.type_list_to_str !outputTypes)
              (FnId.to_str fundef.fn_id)
          in
          raise (TypeError (msg, None))
          ;
    ENDIF;

      while !outputIds <> [] && !outputTypes <> [] do
        Hashtbl.add tenv (List.hd !outputIds) (List.hd !outputTypes);
        outputIds := List.tl !outputIds;
        outputTypes := List.tl !outputTypes
      done
    );
    tenv

  let infer_value_type tenv = function
    | Var id -> get_type tenv id
    | Num n -> Type.ScalarT (ParNum.type_of n)
    | _ -> Type.AnyT

  let value tenv vNode = infer_value_type tenv vNode.value

  let phi_set tenv id t =
    try (
      let oldT = Hashtbl.find tenv id in
      if oldT = t then None
      else
        let t' = Type.common_type oldT t in
        (Hashtbl.replace tenv id t'; Some tenv)
    )
    with _ -> Hashtbl.replace tenv id t; Some tenv

  let phi_merge tenv id tLeft tRight =
    phi_set tenv id (Type.common_type tLeft tRight)

  let rec infer_app tenv fnVal (argTypes:Type.t list) =
    IFDEF DEBUG THEN
      Printf.printf "[TypeAnalysis.infer_app] %s(%s)\n"
        (SSA.value_to_str fnVal)
        (Type.type_list_to_str argTypes)
    ENDIF;
    match fnVal with
    | Prim (Prim.ArrayOp arrayOp) ->
      [TypeInfer.infer_simple_array_op arrayOp argTypes]
    | Prim (Prim.ScalarOp scalarOp) ->
      [TypeInfer.infer_scalar_op scalarOp argTypes]
    | GlobalFn _ ->
      let signature = Signature.from_input_types argTypes in
      P.infer_output_types fnVal signature
    | _ ->
       failwith $
          Printf.sprintf
            "Inference for function application where fn = %s not implemented"
            (SSA.value_to_str fnVal)


  let infer_adverb
       tenv
        ~(adverb:Prim.adverb)
        ~(fn_val:SSA.value)
        ?(closure_arg_types=[])
        ?init
        ?axes
        ~(array_arg_types:Type.t list) =
    IFDEF DEBUG THEN
      Printf.printf
        "[TypeAnalysis] inferring adverb %s over fn %s with args %s\n"
        (Prim.adverb_to_str adverb)
        (SSA.value_to_str fn_val)
        (Type.type_list_to_str array_arg_types)
      ;
    ENDIF;
    if List.for_all Type.is_scalar array_arg_types then
      raise (
        TypeError("Adverbs must have at least one non-scalar argument", None))
    ;
    let maxPossibleAxes =
      SSA_AdverbHelpers.max_num_axes_from_array_types array_arg_types
    in
    let numAxes = match axes with
      | None -> maxPossibleAxes
      | Some axes ->
        let n = List.length axes in
        if n <= maxPossibleAxes then n
        else
        let msg =
          Printf.sprintf
            "Can't have %d axes for adverb %s, max allowed = %d"
            n
            (Prim.adverb_to_str adverb)
            maxPossibleAxes
        in
        raise (TypeError(msg, None))
    in
    match adverb with
    | Prim.Map ->
      if init <> None then
        raise (TypeError("Map can't have initial values", None))
      ;
      let eltTypes = List.map (Type.peel ~num_axes:numAxes) array_arg_types in
      let eltResultTypes = infer_app tenv fn_val eltTypes in
      Type.increase_ranks numAxes eltResultTypes
(*
    | Prim.Reduce ->
      let eltTypes = List.map (Type.peel ~num_axes:numAxes) vecTypes in
      let accTypes = infer_app tenv fnVal (initTypes @ eltTypes) in
      let accTypes' = infer_app tenv fnVal (accTypes @ eltTypes) in
      if accTypes <> accTypes' then
        failwith "unable to infer accumulator type"
      ;
      accTypes
    | Prim.AllPairs ->
        let eltTypes = List.map (Type.peel ~num_axes:numAxes) argTypes in
        let outTypes = infer_app tenv fnVal  eltTypes in
        Type.increase_ranks 2 outTypes
*)
    | other -> failwith (Prim.adverb_to_str other ^ " not impl")

  let exp tenv expNode helpers =
    let src = expNode.exp_src in
    match expNode.exp with
    | App({value=SSA.Prim (Prim.Adverb adverb)}, args) ->
      let argTypes = helpers.eval_values tenv args in
      let fn, args = match args with
        | fn::rest -> fn, rest
        | _ -> raise (TypeError("too few arguments for adverb", src))
      in
      begin match adverb with
        | Prim.Map
        | Prim.AllPairs ->
          infer_adverb
            tenv
            ~adverb
            ~fn_val:fn.value
            ?closure_arg_types:None
            ?init:None
            ?axes:None
            ~array_arg_types:argTypes
        | Prim.Reduce
        | Prim.Scan -> assert false
      end
    | SSA.Adverb(adverb, {closure_fn; closure_arg_types}, {axes;init;args}) ->
      let argTypes = helpers.eval_values tenv args in
      infer_adverb
        tenv
        ~adverb
        ~fn_val:(GlobalFn closure_fn)
        ~closure_arg_types
        ?init:None
        ?axes:None
        ~array_arg_types:argTypes
    | App(lhs, args) ->
        let lhsT = value tenv lhs in
        let argTypes = helpers.eval_values tenv args in
        IFDEF DEBUG THEN
          Printf.printf "[exp] Node: %s Types:%s\n"
            (SSA.exp_to_str expNode)
            (Type.type_list_to_str argTypes);
        ENDIF;
        if Type.is_array lhsT
        then [TypeInfer.infer_simple_array_op Prim.Index (lhsT::argTypes)]
        else infer_app tenv lhs.value argTypes
    | Arr elts ->
        let commonT = Type.combine_type_list (helpers.eval_values tenv elts) in
        if Type.is_scalar commonT then [Type.increase_rank 1 commonT]
        else if commonT = Type.AnyT then
          let errMsg = "Couldn't find unifying type for elements of array" in
          raise  (TypeError (errMsg, expNode.exp_src))
        else
          let errMsg =
            Printf.sprintf
              "Expected array elements to be scalars, got type '%s'"
              (Type.to_str commonT)
          in
          raise (TypeError(errMsg, expNode.exp_src))



    | Values vs -> helpers.eval_values tenv vs
    | _ -> failwith $ Printf.sprintf
            "Type analysis not implemented for expression: %s"
            (SSA.exp_to_str expNode)

  let stmt tenv stmtNode helpers =
    match stmtNode.stmt with
    | Set(ids, rhs) ->
      let types : Type.t list = exp tenv rhs helpers in
      IFDEF DEBUG THEN
        if List.length ids <> List.length types then
          failwith $ sprintf
            "malformed SET statement: %d ids for %d rhs values \n"
            (List.length ids)
            (List.length types)
      ENDIF;
      let rec process_types (tenv, changed) id rhsT =
        IFDEF DEBUG THEN
          if rhsT = Type.AnyT then failwith "error during type inference"
        ENDIF;
        let oldT = get_type_or_bottom tenv id in
        let newT = Type.common_type oldT rhsT in
        let changedT = oldT <> newT in
        let tenv' = if changedT then add_type tenv id newT else tenv in
        tenv', (changed || changedT)
      in
      let tenv', changed =
        List.fold_left2 process_types (tenv, false) ids types
      in
      if changed then Some tenv' else None
   | _ -> helpers.eval_stmt tenv stmtNode
end

let type_analysis
      ~(specializer:SSA.value-> Signature.t -> SSA.fn)
      ~(fn:SSA.fn)
      ~(signature:Signature.t) =
  IFDEF DEBUG THEN
    Printf.printf
      "Specializing %s with signature %s\n"
      (SSA.fn_to_str fn)
      (Signature.to_str signature)
    ;
  ENDIF;
  let module Params : TYPE_ANALYSIS_PARAMS = struct
    let infer_output_types =
      (fun fnVal fnSig -> (specializer fnVal fnSig).fn_output_types)
    let signature = signature
  end
  in
  let module TypeEval = MkEvaluator(MkAnalysis(Params)) in
  TypeEval.eval_fn fn

