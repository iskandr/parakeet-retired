(* pp: -parser o pa_macro.cmo *)
open Printf
open Base
open SSA
open SSA_Helpers
open SSA_Analysis

module type TYPE_ANALYSIS_PARAMS = sig
  val closure_val : ID.t -> value
  val closure_args : ID.t -> value_node list
  val output_arity : value -> int
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
          failwith  $ Printf.sprintf
            "[TypeAnalysis]
                mismatching number of output IDs (%s) and types (%s) in %s"
            (String.concat ", " (List.map ID.to_str !outputIds))
            (Type.type_list_to_str !outputTypes)
            (FnId.to_str fundef.fn_id)
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

  let rec infer_app tenv fnVal (argTypes:Type.t list) = match fnVal with
    | Var id ->
      (* if the identifier would evaluate to a function value...*)
      let fnVal' = P.closure_val id in
      let closureArgNodes = P.closure_args id in
      let closureArgTypes = List.map (value tenv) closureArgNodes in
      infer_app tenv fnVal' (closureArgTypes @ argTypes)
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


  let infer_higher_order tenv arrayOp args argTypes =
    match arrayOp, args, argTypes with
    (* TODO: DEAL WITH AXIS ARGUMENTS! *)
    | Prim.Map, {value=fnVal}::_, _::dataTypes ->
        if List.for_all Type.is_scalar dataTypes then
          failwith "expected at least one argument to map to be a vector"
        ;
        (* we're assuming Map works only along the outermost axis of an array *)
        let eltTypes = List.map Type.peel dataTypes in
        let eltResultTypes = infer_app tenv fnVal eltTypes in
        Type.increase_ranks 1 eltResultTypes

    | Prim.Reduce, {value=fnVal}::_, _::argTypes ->
        let arity = P.output_arity fnVal in
        let initTypes, vecTypes = List.split_nth arity argTypes in
        let eltTypes = List.map Type.peel vecTypes in
        let accTypes = infer_app tenv fnVal (initTypes @ eltTypes) in
        let accTypes' = infer_app tenv fnVal (accTypes @ eltTypes) in
        if accTypes <> accTypes' then
          failwith "unable to infer accumulator type"
        ;
        accTypes
    | Prim.AllPairs, {value=fnVal}::_, _::argTypes ->
        let eltTypes = List.map Type.peel argTypes in
        let outTypes = infer_app tenv fnVal  eltTypes in
        Type.increase_ranks 2 outTypes

    | other, _, _ -> failwith (Prim.adverb_to_str other ^ " not impl")

  let exp tenv expNode helpers =
    match expNode.exp with
    | App({value=SSA.Prim (Prim.Adverb arrayOp)}, args) ->
        infer_higher_order tenv arrayOp args (helpers.eval_values tenv args)
    | App(lhs, args) ->
        let lhsT = value tenv lhs in
        let argTypes = helpers.eval_values tenv args in
        Printf.printf "[exp] Node: %s Types:%s\n"
          (SSA.exp_to_str expNode)
          (Type.type_list_to_str argTypes);
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
    Printf.printf "\n[stmt] %s\n%!" (SSA.stmt_node_to_str stmtNode);
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
      ~(output_arity: SSA.value -> int)
      ~(closureEnv:CollectPartialApps.closure_env)
      ~(fn:SSA.fn)
      ~(signature:Signature.t) =
  Printf.printf "Specializing %s with signature %s\n" (SSA.fn_to_str fn) (Signature.to_str signature);
  let module Params : TYPE_ANALYSIS_PARAMS = struct
    let closure_val =
      (fun id -> Hashtbl.find closureEnv.CollectPartialApps.closures id)
    let closure_args =
      (fun id -> Hashtbl.find closureEnv.CollectPartialApps.closure_args id)
    let output_arity = output_arity
    let infer_output_types =
      (fun fnVal fnSig -> (specializer fnVal fnSig).fn_output_types)
    let signature = signature
  end
  in
  let module TypeEval = MkEvaluator(MkAnalysis(Params)) in
  TypeEval.eval_fn fn

