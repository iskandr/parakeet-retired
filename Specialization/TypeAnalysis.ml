(* pp: -parser o pa_macro.cmo *)
open Printf
open Base
open UntypedSSA


exception TypeError of string * (SrcInfo.t option)


(* TODO: make this complete for all SSA statements *)
let rec is_scalar_stmt = function
  | UntypedSSA.Set(_,
    {exp=UntypedSSA.App({value=UntypedSSA.Prim (Prim.ScalarOp _)}, _)})
  | UntypedSSA.Set(_, {exp=Values _}) -> true
  | UntypedSSA.If(_, tCode, fCode, _) ->
      all_scalar_stmts tCode && all_scalar_stmts fCode
  | _ -> false
and is_scalar_stmt_node stmtNode = is_scalar_stmt stmtNode.stmt
and all_scalar_stmts stmts = Block.for_all is_scalar_stmt_node stmts

(* used this functor parameter to recursively call back into Specialize *)
module type TYPE_ANALYSIS_PARAMS = sig
  val infer_output_types : value -> Signature.t -> Type.t list
end

module Make (P : TYPE_ANALYSIS_PARAMS) = struct

  let tenv : (ID.t, Type.t) Hashtbl.t = Hashtbl.create 127

  let get_type_or_bottom id = Hashtbl.find_default tenv id Type.BottomT
  let get_type id =
    match Hashtbl.find_option tenv id with
    | Some t -> t
    | None ->
      failwith $
      Printf.sprintf "Couldn't find type for variable %s"  (ID.to_str id)

  let add_type id t = Hashtbl.add tenv id t
  let replace_type id t = Hashtbl.replace tenv id t

  let analyze_fn fundef signature =
    Hashtbl.clear tenv;
    let inputIds = ref fundef.input_ids in
    let inputTypes = ref (Signature.input_types signature) in
    IFDEF DEBUG THEN
      if List.length !inputIds <> List.length !inputTypes then
        let errorMsg = Printf.sprintf
          "mismatching number of input IDs (%s) and types (%s) in %s"
          (String.concat ", " (List.map ID.to_str !inputIds))
          (Type.type_list_to_str !inputTypes)
          (FnId.to_str fundef.fn_id)
        in
        raise (TypeError(errorMsg, UntypedSSA.find_fn_src_info fundef))
    ENDIF;

    while !inputIds <> [] && !inputTypes <> [] do
      add_type (List.hd !inputIds) (List.hd !inputTypes);
      inputIds := List.tl !inputIds;
      inputTypes := List.tl !inputTypes
    done;
    if Signature.has_output_types signature then (
      let outputIds = ref fundef.output_ids in
      let outputTypes = ref (Signature.output_types signature) in
      IFDEF DEBUG THEN
        if List.length !outputIds <> List.length !outputTypes then
          let msg =
            Printf.sprintf
              "mismatching number of output IDs (%s) and types (%s) in %s"
              (String.concat ", " (List.map ID.to_str !outputIds))
              (Type.type_list_to_str !outputTypes)
              (FnId.to_str fundef.fn_id)
          in
          raise (TypeError (msg, UntypedSSA.find_fn_src_info fundef))
      ENDIF;
      while !outputIds <> [] && !outputTypes <> [] do
        add_type (List.hd !outputIds) (List.hd !outputTypes);
        outputIds := List.tl !outputIds;
        outputTypes := List.tl !outputTypes
      done
    )

  let infer_value_type = function
    | Var id -> get_type id
    | Num n -> Type.ScalarT (ParNum.type_of n)
    | _ -> Type.AnyT

  let infer_value_node vNode = infer_value_type vNode.value

  let infer_value_nodes vNodes = List.map infer_value_node vNodes

  let phi_set id t =
    match Hashtbl.find_option tenv id with
      | Some oldT ->
        if oldT <> t then
          let t' = Type.common_type oldT t in
          replace_type id t'
      | None -> replace_type id t

  let phi_merge tenv id tLeft tRight =
    phi_set id (Type.common_type tLeft tRight)

  let rec infer_app fnVal (argTypes:Type.t list) =
    IFDEF DEBUG THEN
      Printf.printf "[TypeAnalysis.infer_app] %s(%s)\n"
        (UntypedSSA.value_to_str fnVal)
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
            (UntypedSSA.value_to_str fnVal)


  let infer_adverb
        ?(src:SrcInfo.t option)
        (info:
          (UntypedSSA.value, Type.t list, UntypedSSA.value list option)
          Adverb.info
        )
        (array_arg_types:Type.t list) =
    if List.for_all Type.is_scalar array_arg_types then
      raise (
        TypeError("Adverbs must have at least one non-scalar argument", src))
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
        raise (TypeError(msg, src))
    in
    let eltTypes = List.map (Type.peel ~num_axes:numAxes) array_arg_types in
    match adverb, init, eltTypes with
    | Prim.Map, None, _ ->
      let eltResultTypes = infer_app fn_val eltTypes in
      Type.increase_ranks numAxes eltResultTypes
    | Prim.Map, Some _, _ ->
      raise (TypeError("Map can't have initial values", src))
    (* if not given initial values then we assume operator is binary and*)
    (* used first two elements of the array *)
    | Prim.Reduce, None, [eltT] ->
      let accTypes = infer_app fn_val [eltT;eltT] in
      if List.length accTypes <> 1 then
        raise (
          TypeError("Reduce without inital args must return one value", src))
      else accTypes
    | Prim.Reduce, None, _ ->
      raise (
        TypeError("Reduce without intial args must have one input array", src))
    | Prim.Reduce, Some inits, _  -> assert false

    | other, _, _ -> failwith (Prim.adverb_to_str other ^ " not impl")

  let exp expNode helpers =
    let src = expNode.exp_src in
    match expNode.exp with
    | App({value=UntypedSSA.Prim (Prim.Adverb adverb)}, args) ->
      (********************************************************)
      (* TODO: FIX THIS TO WORK WITH Adverb.info              *)
      let argTypes = infer_value_nodes args in
      let fn, args = match args with
        | fn::rest -> fn, rest
        | _ -> raise (TypeError("too few arguments for adverb", src))
      in
      begin match adverb with
        | Prim.Map
        | Prim.AllPairs ->
          infer_adverb ?src  ~adverb ~fn_val:fn.value
            ?closure_arg_types:None
            ?init:None
            ?axes:None
            ~array_arg_types:argTypes
        | Prim.Reduce
        | Prim.Scan -> assert false
      end
    | UntypedSSA.Adverb(adverb, {closure_fn; closure_arg_types}, {axes;init;args}) ->
      let argTypes = infer_value_nodes args in
      let resultTypes =
        infer_adverb ?src  ~adverb ~fn_val:(GlobalFn closure_fn)
          ~closure_arg_types
          ?init:None
          ?axes:None
          ~array_arg_types:argTypes
      in
      IFDEF DEBUG THEN
        Printf.printf
          "[TypeAnalysis.exp] Inferred output for adverb %s: %s\n"
          (Prim.adverb_to_str adverb)
          (Type.type_list_to_str resultTypes)
        ;
      ENDIF;
      resultTypes
    | App(lhs, args) ->
        let lhsT = infer_value_node lhs in
        let argTypes = infer_value_nodes args in
        IFDEF DEBUG THEN
          Printf.printf "[exp] Node: %s Types:%s\n"
            (UntypedSSA.exp_to_str expNode)
            (Type.type_list_to_str argTypes);
        ENDIF;
        if Type.is_array lhsT
        then [TypeInfer.infer_simple_array_op Prim.Index (lhsT::argTypes)]
        else infer_app lhs.value argTypes
    | Arr elts ->
        let commonT = Type.combine_type_list (infer_value_nodes elts) in
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



    | Values vs -> infer_value_nodes vs
    | _ -> failwith $ Printf.sprintf
            "Type analysis not implemented for expression: %s"
            (UntypedSSA.exp_to_str expNode)

  let rec stmt stmtNode helpers : tenv =
    match stmtNode.stmt with
    | Set(ids, rhs) ->
      let types : Type.t list = exp rhs helpers in
      IFDEF DEBUG THEN
        if List.length ids <> List.length types then
          failwith $ sprintf
            "malformed SET statement: %d ids for %d rhs values \n"
            (List.length ids)
            (List.length types)
      ENDIF;
      let aux id rhsT =
        let oldT = get_type_or_bottom tenv id in
        let newT = Type.common_type oldT rhsT in
        if oldT <> newT then
        if changedT then replace_type id newT
      in
      List.iter2 aux ids types
   | _ -> (*CANT RELY ON HELPERS ANY MORE!*)
end

let type_analysis
      ~(specializer:UntypedSSA.value-> Signature.t -> UntypedSSA.fn)
      ~(fn:UntypedSSA.fn)
      ~(signature:Signature.t) =
  IFDEF DEBUG THEN
    Printf.printf
      "Specializing %s with signature %s\n"
      (UntypedSSA.fn_to_str fn)
      (Signature.to_str signature)
    ;
  ENDIF;
  let module Params : TYPE_ANALYSIS_PARAMS = struct
    let infer_output_types =
      (fun fnVal fnSig -> (specializer fnVal fnSig).fn_output_types)
  end
  in
  let module TypeEval = Make(Params) in
  TypeEval.analyze_fn fn signature

