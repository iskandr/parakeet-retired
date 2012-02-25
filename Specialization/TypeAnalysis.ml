(* pp: -parser o pa_macro.cmo *)
open Printf
open Base
open Type
open UntypedSSA


exception TypeError of string * (SrcInfo.t option)


let infer_unop (op:Prim.scalar_op) (t:Type.t) : Type.t =
  let resultScalarT = Type.common_elt_type (Type.elt_type t) Float32T in
  if Prim.is_float_unop op then Type.fill_elt_type t resultScalarT
  else if op = Prim.Not then fill_elt_type t BoolT
  else t

let infer_binop (op:Prim.scalar_op) (t1:Type.t) (t2:Type.t) : Type.t  =
  match Type.common_type t1 t2 with
  | AnyT ->
    let errMsg =
       sprintf
        "[infer_binop] Cannot find concrete common type for %s and %s"
        (Type.to_str t1)
        (Type.to_str t2)
    in
    raise TypeError(errMsg, None)
  | t3 ->
    if Prim.is_comparison op then Type.fill_elt_type t3 BoolT
    else if Prim.is_float_binop op then
        let eltResultT =
            if Type.sizeof (Type.elt_type t3) <= Type.sizeof Float32T
            then Float32T
            else Float64T
        in
        fill_elt_type  t3 eltResultT
    else t3

let infer_select predT t1 t2 =
  if common_type predT Type.bool = AnyT then
    raise (
      TypeError("predicate for Select operator must be a subtype of Bool", None)
    )
  else match common_type t1 t2 with
    | AnyT ->
      let errMsg =
        sprintf
          "[infer_op] Scalar conditional cannot match %s and %s\n%!"
          (Type.to_str t1)
          (Type.to_str t2)
      in
      raise TypeError(errMsg, None)
    | t3 ->
      if Type.rank predT = Type.rank t3 then t3
      else
        raise $
          TypeError("nest depth of predicate must match rank of value", None)

let infer_scalar_op op argTypes = match op, argTypes with
  | op, [t1;t2] when Prim.is_binop op -> infer_binop op t1 t2
  | op, [t] when Prim.is_unop op -> infer_unop op t
  | Prim.Select, [predT; t1; t2] -> infer_select predT t1 t2
  | other, types ->
    let errMsg =
      Printf.sprintf
        "can't infer type for %s with args %s, not a scalar operation"
        (Prim.scalar_op_to_str other)
        (Type.type_list_to_str types)
    in
    raise TypeError(errMsg, None)

let infer_indexing_result eltT rank indexTypes =
  let nIndices = List.length indexTypes in
  let resultRank = rank - nIndices in
  (* this will be the result if we're indexing only scalars *)
  if List.for_all Type.is_scalar indexTypes then
    Type.mk_array_type eltT resultRank
  else match indexTypes with
    | [Type.ArrayT(BoolT, 1)]
    | [Type.ArrayT(Int32T, 1)] -> Type.ArrayT(eltT, rank)
    | _ ->
      let errMsg =
        Printf.sprintf
          "[TypeInfer] unsupported indices: %s"
          (Type.type_list_to_str indexTypes)
      in
      raise (TypeError(errMsg, None))

let infer_simple_array_op op argTypes = match op, argTypes with
  | Prim.Range, [t] when Type.is_scalar t  ->
      if Type.common_type t Type.int32  <> AnyT then Type.ArrayT(Int32T, 1)
      else failwith "operator 'til' requires an integer argument"
  | Prim.Range, [_] ->
    failwith "scalar argument expected for operator 'til'"
  | Prim.Range, _ ->
    raise $ TypeError ("wrong arity", None)
  | Prim.Where, [Type.ArrayT(BoolT, 1)] -> Type.ArrayT(Int32T, 1)
  | Prim.Where, _ ->
    raise $ TypeError("operator 'where' expects a vector of booleans", None)
  | Prim.Index, Type.ArrayT(eltT, rank)::indexTypes ->
    infer_indexing_result eltT rank indexTypes
  | Prim.Index, [t; _] when Type.is_scalar t ->
    raise $ TypeError ("can't index into a scalar", None)
  | Prim.DimSize, _ -> Type.ScalarT Int32T
  | Prim.Find,  [Type.ArrayT(elt_t1, 1); ScalarT elt_t2] ->
    assert (elt_t1 = elt_t2);
    Type.ScalarT elt_t1
  | _ ->
    let errMsg =
      Printf.sprintf "Could not infer type for %s" (Prim.array_op_to_str op)
    in
    raise $ TypeError(errMsg, None)

(* given an operator and types of its arguments, return list of types to which *)
(* args must be converted for the operator to work properly *)
let required_scalar_op_types op argtypes =
    match (op, argtypes) with
      (* division returns a float *)
      | op, [t1; t2] when Prim.is_float_binop op ->
          let t3 = Type.common_type t1 t2 in
          let resultT =
            if Type.sizeof (Type.elt_type t3) <= Type.sizeof Float32T
            then Type.float32
            else Type.float64
          in

          [resultT; resultT]

      | op, [t1; t2] when Prim.is_binop op ->
          let t3 = Type.common_type t1 t2 in
          [t3; t3]
      | Prim.Select, [predT; t1; t2] ->
          let t3 = Type.common_type t1 t2 in
          let predT' = Type.common_type predT Type.bool in
          [predT'; t3; t3]
      (* upconvert non-float arguments to appropriate float size *)
      | op, [t]  when Prim.is_float_unop op &&
          Type.sizeof (Type.elt_type t) <= Type.sizeof Float32T-> [Type.float32]
      (* if type doesn't fit in float32 but is scalar use float64 *)
      | op, [t] when Prim.is_float_unop op && Type.is_scalar t -> [Type.float64]
      (* if not a floating unop, just keep type the same *)
      | op, [t] when Prim.is_unop op -> [t]
      | _ ->
        let errMsg =
          Printf.sprintf
            "no valid coercions for operator %s with input types %s"
            (Prim.scalar_op_to_str op) ^
            (Type.type_list_to_str argtypes)
        in
        raise (TypeError(errMsg, None))

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

(* Eek, a mutable type environment! Ain't it the devil? *)
module TypeEnv : sig
  val reset : unit -> unit
  val get_tenv : unit -> (ID.t, Type.t) Hashtbl.t
  val get_type : ID.t -> Type.t
  val get_type_or_bottom : ID.t -> Type.t
  val add_type : ID.t -> Type.t -> unit
  val merge_type : ID.t -> Type.t -> unit
  val version : unit -> int
  val changed_since : int -> bool
  val find_option : ID.t -> Type.t option
end = struct
  let tenv : (ID.t, Type.t) Hashtbl.t = Hashtbl.create 127
  let get_tenv () = tenv
  let reset () = Hashtbl.clear tenv
  let get_type_or_bottom id = Hashtbl.find_default tenv id Type.BottomT
  let get_type id =
    match Hashtbl.find_option tenv id with
    | Some t -> t
    | None ->
      let errMsg =
        Printf.sprintf "Couldn't find type for variable %s"  (ID.to_str id)
      in
      raise $ TypeError(errMsg, None)

  (* uses this to check whether the type env has changed or not *)
  let tenv_version = ref 0
  let version () = !tenv_version
  let changed_since v = version() <> v
  let really_add_type id t =
    tenv_version := !tenv_version + 1;
    Hashtbl.add tenv id t
  let add_type id t =
    if Hashtbl.mem tenv id then
      let errMsg =
        Printf.sprintf
          "Can't add %s : %s to type environment, binding already exists"
          (ID.to_str id)
          (Type.to_str t)
      in
      raise $ TypeError(errMsg, None)
    else really_add_type id t

  let merge_type id t =
    match Hashtbl.find_option tenv id with
      | None -> really_add_type id t
      | Some oldT ->
        let commonT = Type.common_type oldT t in
        if t <> commonT then really_add_type id commonT
  let find_option id  =
    Hashtbl.find_option tenv id
end
(* used this functor parameter to recursively call back into Specialize *)
module type TYPE_ANALYSIS_PARAMS = sig
  val infer_output_types : value -> Signature.t -> Type.t list
end

module Make (P : TYPE_ANALYSIS_PARAMS) = struct
  let infer_value_type = function
    | Var id -> TypeEnv.get_type id
    | Num n -> Type.ScalarT (ParNum.type_of n)
    | _ -> Type.AnyT

  let infer_value_node vNode = infer_value_type vNode.value

  let infer_value_nodes vNodes = List.map infer_value_node vNodes

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
      AdverbHelpers.max_num_axes_from_array_types array_arg_types
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

  let infer_exp expNode =
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



  let init_phi_node {PhiNode.phi_id; phi_left} =
    TypeEnv.add_type phi_id (infer_value_node phi_left)
  let init_phi_nodes phiNodes = List.iter init_phi_node phiNodes

  let analyze_phi_node {PhiNode.phi_id; phi_left; phi_right} =
    let tLeft = infer_value_node phi_left in
    let tRight = infer_value_node phi_right in
    TypeEnv.merge_type phi_id (Type.common_type tLeft tRight)

  let analyze_phi_nodes phiNodes = List.iter analyze_phi_nodes phiNodes

  let rec analyze_stmt (stmtNode:UntypedSSA.stmt_node) : unit =
    let src = stmtNode.stmt_src in
    match stmtNode.stmt with
    | Set(ids, rhs) ->
      let types : Type.t list = infer_exp rhs in
      IFDEF DEBUG THEN
        if List.length ids <> List.length types then
          failwith $ sprintf
            "malformed SET statement: %d ids for %d rhs values \n"
            (List.length ids)
            (List.length types)
      ENDIF;
      List.iter2 TypeEnv.replace_type ids types
    | If(_, tBlock, fBlock, phiNodes) ->
        analyze_block tBlock;
        analyze_block fBlock;
        analyze_phi_nodes phiNodes
    | WhileLoop(condBlock, _, body, phiNodes) ->
       let old_type_env_version = ref (TypeEnv.version()) in
       init_phi_nodes phiNodes;
       let maxIters = 100 in
       let iter = ref 0 in
       while !old_type_env_version <> TypeEnv.version() do
         old_type_env_version := TypeEnv.version ();
         iter := !iter + 1;
         if !iter > maxIters then
           raise $ TypeError("loop analysis failed to terminate", src)
         else (
            analyze_block condBlock;
            analyze_block block;
            analyze_phi_nodes phiNodes
        )
      done
  and analyze_block (block:UntypedSSA.block) : unit =
    Block.iter_forward analyze_stmt block

  let analyze_fn fundef signature =
    TypeEnv.reset();
    let inputIds = fundef.input_ids in
    let inputTypes = Signature.input_types signature in
    IFDEF DEBUG THEN
      if List.length inputIds <> List.length inputTypes then
        let errorMsg = Printf.sprintf
          "mismatching number of input IDs (%s) and types (%s) in %s"
          (ID.list_to_str inputIds)
          (Type.type_list_to_str inputTypes)
          (FnId.to_str fundef.fn_id)
        in
        raise (TypeError(errorMsg, UntypedSSA.find_fn_src_info fundef))
    ENDIF;
    List.iter2 TypeEnv.add_type inputIds inputTypes;
    if Signature.has_output_types signature then begin
      let outputIds = fundef.output_ids in
      let outputTypes = Signature.output_types signature in
      IFDEF DEBUG THEN
        if List.length outputIds <> List.length outputTypes then
          let msg =
            Printf.sprintf
              "mismatching number of output IDs (%s) and types (%s) in %s"
              (ID.list_to_str outputIds)
              (Type.type_list_to_str outputTypes)
              (FnId.to_str fundef.fn_id)
          in
          raise (TypeError (msg, UntypedSSA.find_fn_src_info fundef))
      ENDIF;
      List.iter2 TypeEnv.add_type outputIds outputTypes
    end;
    analyze_block fundef.body;
    TypeEnv.get_tenv()
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

