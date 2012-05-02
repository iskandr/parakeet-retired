(* pp: -parser o pa_macro.cmo *)

open Base
open AdverbHelpers
open SSA_Codegen
open Type
open Printf

(* make a fresh function definition whose body is only an untyped prim *)
let untypedPrimFnCache : (Prim.t * int, UntypedSSA.fn) Hashtbl.t =
  Hashtbl.create 127

let mk_untyped_prim_fn (prim:Prim.t) arity : UntypedSSA.fn =
  let key = (prim,arity) in
  if Hashtbl.mem untypedPrimFnCache key  then
    Hashtbl.find untypedPrimFnCache key
  else
  let inputs = ID.gen_named_list "input" arity in
  let output = ID.gen_named "output" in
  let inputVars = List.map UntypedSSA.var inputs in
  let rhs =
    UntypedSSA.call 
      (UntypedSSA.wrap_value (UntypedSSA.Prim prim)) 
      (Args.of_values inputVars)
  in
  let body = Block.singleton (UntypedSSA.set [output] rhs) in
  let fn =
    UntypedSSA.mk_fn
      ~name:("prim" ^ (Prim.to_str prim))
      ~input_ids:inputs
      ~output_ids:[output]
      ~body
  in
  (Hashtbl.add untypedPrimFnCache key fn; fn)

let mk_typed_scalar_prim (op : Prim.scalar_op) ?optOutType argTypes =
  let reqArgTypes =  TypeAnalysis.required_scalar_op_types op argTypes in
  let inferredOutType = TypeAnalysis.infer_scalar_op op reqArgTypes in
  let name =
    Printf.sprintf "prim%s{%s%s}"
      (Prim.scalar_op_to_str op)
      (Type.type_list_to_str ~sep:"," argTypes)
      (match optOutType with None -> "" | Some t -> "=>" ^ (Type.to_str t))
  in
  let outType = match optOutType with
    | Some t -> t
    | None -> inferredOutType
  in
  SSA_Codegen.mk_codegen_fn ~name argTypes [outType] $
    fun codegen inputs outputs ->
      let args = Array.of_list inputs in
      let inTyArr = Array.of_list argTypes in
      let reqTyArr = Array.of_list reqArgTypes in
      for i = 0 to Array.length args - 1 do
        let reqT = reqTyArr.(i) in
        if inTyArr.(i) <> reqT then begin
          let id = codegen#fresh_var reqT in
          codegen#emit [TypedSSA.set [id]  (TypedSSA.cast reqT args.(i))];
          args.(i) <- codegen#id_value_node id
        end
      done
      ;
      let primAppNode =
        TypedSSA.primapp (Prim.ScalarOp op) [outType] (Array.to_list args)
      in
      let outputVar = List.hd outputs in
      codegen#emit [TypedSSA.set [TypedSSA.get_id outputVar] primAppNode]

(* 1) some statements (ie, those involving array ops)
      make a function definitely not a scalar-only function.
      In a three-value-logic, these give the whole function a value of No.
   2) Other statements have a neutral effect (ie, setting constants).
      These have a value Maybe.
*)




let rec specialize_fn fn signature =
  IFDEF DEBUG THEN
    Printf.printf
      "[Specialize.specialize_fn] %s :: %s\n"
      (FnId.to_str fn.UntypedSSA.fn_id)
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
    (UntypedSSA.ScalarHelpers.is_scalar_fn ~control_flow:false fn)
  then scalarize_fn fn signature
  else
  (* to avoid having to make RewriteTyped and Specialize recursive
       modules I've untied the recursion by making specialize_value
       a parameter
   *)
  RewriteTyped.rewrite_typed ~specializer:specialize_value ~fn:fn ~signature

and scalarize_fn untyped vecSig =
  IFDEF DEBUG THEN
    Printf.printf "[Specialize.scalarize_fn] %s :: %s\n"
      (FnId.to_str untyped.UntypedSSA.fn_id)
      (Signature.to_str vecSig);
  ENDIF;
  let inTypes = Signature.input_types vecSig in
  let numAxes = AdverbHelpers.max_num_axes_from_array_types inTypes in
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
  let scalarFn =
    specialize_value (UntypedSSA.GlobalFn untyped.UntypedSSA.fn_id) scalarSig
  in
  let scalarOutputTypes = scalarFn.TypedSSA.fn_output_types in
  let outTypes = List.map (Type.increase_rank numAxes) scalarOutputTypes in
  let adverbInfo : (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info  = {
    Adverb.adverb = Adverb.Map;
    adverb_fn = scalarFn.TypedSSA.fn_id;
    axes = AdverbHelpers.infer_adverb_axes_from_types inTypes;
    fixed_args = [];
    init = None;
    array_args = inTypes;
  }
  in
  AdverbHelpers.mk_adverb_fn adverbInfo

and specialize_value fnVal signature =
  IFDEF DEBUG THEN
    Printf.printf "[Specialize.specialize_value] %s :: %s\n%!"
      (UntypedSSA.value_to_str fnVal)
      (Signature.to_str signature)
    ;
  ENDIF;
  match FnManager.maybe_get_specialization fnVal signature with
  | Some fnId -> FnManager.get_typed_function fnId
  | None ->
    let inputTypes = Signature.input_types signature in
    (match fnVal with
      | UntypedSSA.GlobalFn fnId ->
        let untyped = FnManager.get_untyped_function fnId in
        let typed = specialize_fn untyped signature in
        FnManager.add_specialization ~optimize:true fnVal signature typed;
        typed
      | UntypedSSA.Prim (Prim.ScalarOp op) ->
        let optOutType =
          Option.map List.hd (Signature.output_types_option signature)
        in
        if List.for_all Type.is_scalar inputTypes then (
          let typedFn = mk_typed_scalar_prim op ?optOutType inputTypes in
          FnManager.add_specialization ~optimize:false fnVal signature typedFn;
          typedFn
        )
        else begin
          (* if we're adding two arrays, then turn it into a map over both *)
          (* axes, but if we're adding an array to a vector we can only map*)
          (* over one axis *)
          let maxRank =
            AdverbHelpers.max_num_axes_from_array_types inputTypes
          in
          let nestedInputTypes =
            List.map (Type.peel ~num_axes:maxRank) inputTypes
          in
          let nestedSig =
            match optOutType with
            | None -> Signature.from_input_types nestedInputTypes
            | Some outT ->
              let nestedOutTypes = [Type.peel ~num_axes:maxRank outT] in
              Signature.from_types nestedInputTypes nestedOutTypes
          in
          let nestedFn = specialize_value fnVal nestedSig in
          let adverbInfo = {
            Adverb.adverb = Adverb.Map;
            adverb_fn = nestedFn.TypedSSA.fn_id;
            axes = AdverbHelpers.infer_adverb_axes_from_rank maxRank;
            fixed_args = [];
            init = None;
            array_args = inputTypes;
          }
          in
          let typedFn = AdverbHelpers.mk_adverb_fn adverbInfo in
          FnManager.add_specialization ~optimize:false fnVal signature typedFn;
          typedFn
        end

      | UntypedSSA.Prim p ->
          let arity = List.length inputTypes in
          assert (arity >= Prim.min_prim_arity p &&
                  arity <= Prim.max_prim_arity p);
          let untyped = mk_untyped_prim_fn p arity in
          let typed = specialize_fn untyped signature in
          FnManager.add_specialization ~optimize:false  fnVal signature typed;
          typed
      | _ -> assert false
    )

let specialize_fn_id fnId signature =
  specialize_value (UntypedSSA.GlobalFn fnId) signature
