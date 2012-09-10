(* pp: -parser o pa_macro.cmo *)
 
open Printf
open Base
open Type
open Adverb
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
    raise $ TypeError(errMsg, None)
  | t3 ->
    if Prim.is_comparison op then Type.fill_elt_type t3 BoolT
    (*else if Prim.is_float_binop op then
        let eltResultT =
            if Type.sizeof (Type.elt_type t3) <= Type.sizeof Float32T
            then Float32T
            else Float64T
        in
        fill_elt_type  t3 eltResultT
    *)
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
      raise $ TypeError(errMsg, None)
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
    raise $ TypeError(errMsg, None)

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
  | Prim.Index, Type.ArrayT(eltT, rank)::[Type.TupleT indexTypes] 
  | Prim.Index, Type.ArrayT(eltT, rank)::indexTypes ->
    infer_indexing_result eltT rank indexTypes
  | Prim.Index, [t; _] when Type.is_scalar t ->
    raise $ TypeError ("can't index into a scalar", None)
  | Prim.DimSize, _ -> Type.ScalarT Int32T
  | Prim.Find, [Type.ArrayT(elt_t1, 1); ScalarT elt_t2] ->
    assert (elt_t1 = elt_t2);
    Type.ScalarT elt_t1
  | Prim.Shape, [_] ->
    (* shape operator always returns a 1D shape vector *)
    Type.ArrayT(Type.Int32T, 1)
  | Prim.Transpose, [t] -> t
  | _, ts ->
    let errMsg =
      Printf.sprintf
        "Could not infer type for array op %s with arguments: %s"
        (Prim.array_op_to_str op)
        (Type.type_list_to_str ts)
    in
    raise $ TypeError(errMsg, None)

(* given an operator and types of its arguments, return list of types to which *)
(* args must be converted for the operator to work properly *)
let required_scalar_op_types op argtypes =
    match (op, argtypes) with
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
          (Prim.scalar_op_to_str op)
          (Type.type_list_to_str argtypes)
      in
      raise (TypeError(errMsg, None))


 let infer_num_axes ?src ?axes arrayTypes : int =
    let maxPossibleAxes =
      AdverbHelpers.max_num_axes_from_array_types arrayTypes
    in
   match axes with
    | None -> maxPossibleAxes
    | Some axes ->
      let n = List.length axes in
      if n <= maxPossibleAxes then n
      else
        let msg =
          Printf.sprintf
            "Can't have %d axes, max allowed = %d" n maxPossibleAxes
        in
        raise (TypeError(msg, src))

(* factored out all the error conditions on adverbs *)
let check_adverb_error ?src adverb init eltTypes : unit =
  match adverb, init, eltTypes with
  | Adverb.Reduce, None, eltTypes ->
    if List.length eltTypes <> 1 then
      raise $
        TypeError("Reduce without intial args must have one input array", src)


 
  | Adverb.Map, Some _, _ ->
    raise (TypeError("Map can't have initial values", src))
  | Adverb.Reduce, Some inits, _  ->
    failwith "Reduce with inits not implemented"
  | Adverb.Scan, _, _ -> failwith "Scan not implemented"
  | _ -> failwith "[TypeAnalysis] Invalid adverb"

(* the output of an adverb depends only on the output type of its *)
(* parameterizing function and the number of axes *)
let infer_adverb_result_types  ~adverb_type ~elt_result_types ~num_axes =
  match adverb_type with
    | Adverb.Scan
    | Adverb.Map -> Type.increase_ranks num_axes elt_result_types
  
    | Adverb.Reduce -> elt_result_types

(* Eek, a mutable type environment! Ain't it the devil? *)
module TypeEnv : sig
  val push : unit -> unit
  val pop : unit -> (ID.t, Type.t) Hashtbl.t
  val get_type : ID.t -> Type.t
  val get_type_or_bottom : ID.t -> Type.t
  val add_type : ID.t -> Type.t -> unit
  val merge_type : ID.t -> Type.t -> unit
  val version : unit -> int
  val changed_since : int -> bool
  val find_option : ID.t -> Type.t option
end = struct
  type tenv = (ID.t, Type.t) Hashtbl.t
  let tenv_stack : tenv Stack.t = Stack.create()
  let push () = Stack.push (Hashtbl.create 127) tenv_stack
  let pop () = Stack.pop tenv_stack
  let top () = Stack.top tenv_stack

  let get_type_or_bottom id = Hashtbl.find_default (top()) id Type.BottomT
  let get_type id =
    match Hashtbl.find_option (top()) id with
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
    Hashtbl.add (top()) id t
  let add_type id t =
    IFDEF DEBUG THEN
      Printf.printf "[TypeEnv] Adding %s : %s\n%!"
        (ID.to_str id)
        (Type.to_str t)
    ENDIF;
    if Hashtbl.mem (top()) id then
      let errMsg =
        Printf.sprintf
          "Can't add %s : %s to type environment, binding already exists"
          (ID.to_str id)
          (Type.to_str t)
      in
      raise $ TypeError(errMsg, None)
    else really_add_type id t

  let merge_type id t =
    match Hashtbl.find_option (top()) id with
      | None -> really_add_type id t
      | Some oldT ->
        let commonT = Type.common_type oldT t in
        if t <> commonT then really_add_type id commonT
  let find_option id  =
    Hashtbl.find_option (top()) id
end
(* used this functor parameter to recursively call back into Specialize *)
module type TYPE_ANALYSIS_PARAMS = sig
  val infer_output_types : value -> Signature.t -> Type.t list
end

module Make (P : TYPE_ANALYSIS_PARAMS) = struct
  let infer_value_type = function
    | Var id -> TypeEnv.get_type id
    | Num n -> Type.ScalarT (ParNum.type_of n)
    | NoneVal -> Type.NoneT 
    | _ -> Type.AnyT

  let infer_value_node vNode = infer_value_type vNode.value

  let infer_value_nodes vNodes = List.map infer_value_node vNodes

  let infer_args {Args.values; keywords} = 
    { Args.values = infer_value_nodes values; 
      keywords = 
        List.map 
          (fun (name,v) -> (name, infer_value_node v))
        keywords
    }
  let rec infer_call 
    (fnVal : UntypedSSA.value)
    (argTypes: Type.t Args.actual_args) =
    (* for calls to prims, ignore any keyword arguments *) 
    match fnVal with
    | Prim (Prim.ArrayOp arrayOp) ->
      assert (argTypes.Args.keywords = []); 
      [infer_simple_array_op arrayOp argTypes.Args.values]
    | Prim (Prim.ScalarOp scalarOp) ->
      assert (argTypes.Args.keywords = []); 
      [infer_scalar_op scalarOp argTypes.Args.values]
    | GlobalFn _ ->
      let signature = Signature.from_args argTypes in
      P.infer_output_types fnVal signature
    | _ ->
       failwith $
          Printf.sprintf
            "Inference for function application where fn = %s not implemented"
            (UntypedSSA.value_to_str fnVal)


  let infer_adverb
        ?(src:SrcInfo.t option)
        (info: (UntypedSSA.value, Type.t list, int) Adverb.t)
        : Type.t list =
    if List.for_all Type.is_scalar info. args then
      raise (
        TypeError("Adverbs must have at least one non-scalar argument", src))
    ;
    let numAxes = info.axes in
    let eltTypes = List.map (Type.peel ~num_axes:numAxes) info. args in
    let fnVal : UntypedSSA.value = info.fn in
    let eltResultTypes =
      match info.adverb_type, info.init, eltTypes with
      | Adverb.Map, None, _  ->
        infer_call fnVal (Args.of_values $ info.fixed @ eltTypes)
      (* if not given initial values then we assume operator is binary and*)
      (* used first two elements of the array *)
      | Adverb.Reduce, Some inits, _ -> 
        infer_call fnVal (Args.of_values $ info.fixed @ inits @ eltTypes)
      | Adverb.Reduce, None, [eltT]  ->
        let accTypes = 
          infer_call fnVal 
            (Args.of_values $ info.fixed @ [eltT;eltT]) 
        in
        if List.length accTypes <> 1 then
          raise $
            TypeError("Reduce without inital args must return one value", src)
        else accTypes
      | _ -> 
        check_adverb_error ?src info.adverb_type info.init eltTypes; 
        []
    in
    infer_adverb_result_types info.adverb_type eltResultTypes numAxes

  let infer_exp expNode : Type.t list =
    let src = expNode.exp_src in
    match expNode.exp with
    (*
    | Call(
        {value=UntypedSSA.Prim (Prim.Adverb adverb)}, 
        args) ->
      let fn, arrayArgs = 
        match args.Args.values with
        | fn::rest -> fn, rest
        | _ -> raise (TypeError("too few arguments for adverb", src))
      in
      let arrayTypes = infer_value_nodes arrayArgs in
      infer_adverb ?src {
        adverb_type = (match;
        fn = fn.value;
        combine = None; 
        args = arrayTypes;
        init = None;
        axes = infer_num_axes ?src ?axes:None arrayTypes;
        fixed = [];
      }
    *)
    | Adverb adverb ->
      let arrayTypes = infer_value_nodes adverb.args in
      infer_adverb ?src $
          Adverb.apply_to_fields
            adverb
            ~fn:(fun valNode -> valNode.value)
            ~values:infer_value_nodes
            ~axes:(fun axes -> infer_num_axes ?src ?axes arrayTypes)
      
    | Call(lhs, args) ->
      let lhsT = infer_value_node lhs in
      let argTypes : Type.t Args.actual_args = infer_args args in
      if Type.is_array lhsT
      then [
        infer_simple_array_op Prim.Index (lhsT::argTypes.Args.values)
      ]
      else infer_call lhs.value argTypes
    | Array elts ->
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
    | Tuple vs -> [Type.TupleT (infer_value_nodes vs)] 
    | _ -> failwith $ Printf.sprintf
            "Type analysis not implemented for expression: %s"
            (UntypedSSA.exp_node_to_str expNode)

  let init_phi_node {PhiNode.phi_id; phi_left} =
    TypeEnv.add_type phi_id (infer_value_node phi_left)

  let init_phi_nodes phiNodes = List.iter init_phi_node phiNodes

  let analyze_phi_node {PhiNode.phi_id; phi_left; phi_right} =
    let tLeft = infer_value_node phi_left in
    let tRight = infer_value_node phi_right in
    TypeEnv.merge_type phi_id (Type.common_type tLeft tRight)

  let analyze_phi_nodes phiNodes = List.iter analyze_phi_node phiNodes

  let rec analyze_stmt (stmtNode:UntypedSSA.stmt_node) : unit =
    (*IFDEF DEBUG THEN
      Printf.printf "[TypeAnalysis.infer_stmt] %s\n%!"
        (UntypedSSA.PrettyPrinters.stmt_node_to_str stmtNode)
    ENDIF;*)
    let src = stmtNode.stmt_src in
    match stmtNode.stmt with
    | Set(ids, rhs) ->
      let types : Type.t list = infer_exp rhs in
      (*IFDEF DEBUG THEN
        Printf.printf "  RHS types: %s\n%!" (Type.type_list_to_str types);
        if List.length ids <> List.length types then
          failwith $ sprintf
            "malformed SET statement: %d ids for %d rhs values \n"
            (List.length ids)
            (List.length types)
      ENDIF;*)
      List.iter2 TypeEnv.merge_type ids types
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
           analyze_block body;
           analyze_phi_nodes phiNodes
        )
      done
    | SetIdx (arr, indices, rhs) ->
      (* assume everything else about arr, indices, rhs will be figured out*)
      (* elsewhere *)
      ()

  and analyze_block (block:UntypedSSA.block) : unit =
    Block.iter_forward analyze_stmt block
      
  let analyze_fn fundef signature =
    TypeEnv.push();
    
    let argPairs = 
      Args.bind 
        (Args.apply_to_formal_values infer_value_node fundef.inputs)
        (Signature.inputs signature)
    in
    let () = 
      List.iter 
        (fun (name,t) ->
          let id = String.Map.find name fundef.input_names_to_ids in
          TypeEnv.add_type id t
        )
        argPairs
    in
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
          raise (TypeError (msg, None))
      ENDIF;
      List.iter2 TypeEnv.add_type outputIds outputTypes
    end;
    analyze_block fundef.body;
    let tenv = TypeEnv.pop() in
    IFDEF DEBUG THEN
      Printf.printf "[TypeAnalysis] Inferred types for %s(%s):\n%!"
        (UntypedSSA.PrettyPrinters.fn_id_to_str fundef)
        (Signature.to_str signature)
      ;

      Hashtbl.iter
        (fun id t -> Printf.printf "  -- %s : %s\n"
          (ID.to_str id) (Type.to_str t))
        tenv
      ;
    ENDIF;
    tenv
end

let type_analysis
      ~(specializer:UntypedSSA.value-> Signature.t -> TypedSSA.fn)
      ~(fn:UntypedSSA.fn)
      ~(signature:Signature.t) : (ID.t, Type.t) Hashtbl.t =
  IFDEF DEBUG THEN
    Printf.printf
      "Inferring types for %s with signature %s\n"
      (FnId.to_str fn.UntypedSSA.fn_id)
      (Signature.to_str signature)
    ;
  ENDIF;
  let module Params : TYPE_ANALYSIS_PARAMS = struct
    let infer_output_types =
      (fun fnVal fnSig -> TypedSSA.output_types (specializer fnVal fnSig))
  end
  in
  let module TypeEval = Make(Params) in
  TypeEval.analyze_fn fn signature
