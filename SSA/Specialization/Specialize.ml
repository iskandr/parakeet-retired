(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Helpers
open SSA_Codegen
open Type
open Printf

(* make a fresh function definition whose body is only an untyped prim *)
let untypedPrimFnCache : (Prim.prim * int, SSA.fn) Hashtbl.t =
  Hashtbl.create 127

let mk_untyped_prim_fn prim arity : SSA.fn =
  let key = (prim,arity) in
  if Hashtbl.mem untypedPrimFnCache key  then
    Hashtbl.find untypedPrimFnCache key
  else
  let inputs = ID.gen_fresh_list arity in
  let output = ID.gen() in
  let bottoms = List.map (fun _ -> Type.BottomT) inputs in
  let inputVars = List.map (fun id -> mk_var id) inputs in
  let rhs = mk_app (mk_val (Prim prim)) inputVars in
  let body = Block.singleton (mk_set [output] rhs) in
  let fn = mk_fn inputs [output] body in
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
        codegen#emit [mk_set [id]  (mk_cast reqT args.(i))];
        args.(i) <- codegen#id_value_node id
      end
    done
    ;
    let primAppNode =
      mk_primapp
        (Prim.ScalarOp op)
        [outType]
        (Array.to_list args)
    in
    let outputVar = List.hd outputs in
    codegen#emit [[outputVar] := primAppNode]

let mk_typed_map_fn ?src nestedfn inputTypes =
  let nestedOutputTypes = nestedfn.SSA.fn_output_types in
  let outTypes = Type.increase_ranks 1  nestedOutputTypes in
  let closure = mk_closure nestedfn [] in
  SSA_Codegen.mk_codegen_fn inputTypes outTypes $ fun codegen inputs outputs ->
    codegen#emit [outputs := (mk_map ?src closure inputs )]


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

let rec output_arity closures = function
  | Var id ->
      if Hashtbl.mem closures id then
        let fnVal = Hashtbl.find closures id in
        output_arity closures fnVal
      else
        failwith $ Printf.sprintf
          "[Specialize] Couldn't find %s in closures hashtbl"
          (FnId.to_str id)
  | GlobalFn fnId ->
      let fn =
        if FnManager.is_untyped_function fnId
        then FnManager.get_untyped_function fnId
        else FnManager.get_typed_function fnId
      in
      List.length fn.fn_output_types
  | Prim p -> 1
  | _ -> assert false


let rec specialize_fn fn signature =
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
  (* TODO: *)
  (* Get rid of CollectPartialApps, change RewriteTyped to *)
  (* raise an error if you don't supply enough args *)
  let fnIGNORED, closureEnv =
    CollectPartialApps.collect_partial_apps fn
  in

  let outputArity : SSA.value -> int =
    output_arity closureEnv.CollectPartialApps.closures
  in
  (* to avoid having to make TypeAnalysis and Specialize recursive
       modules I've untied the recursion by making specialize_value
       a parameter of TypeAnalysis.
   *)
  let tenv =
    TypeAnalysis.type_analysis
      ~specializer:specialize_value
      ~output_arity:outputArity
      ~closureEnv
      ~fn:fn
      ~signature
  in
  let typedFn =
    RewriteTyped.rewrite_typed
      ~tenv
      ~closureEnv
      ~specializer:specialize_value
      ~output_arity:outputArity
      ~fn:fn
  in
  typedFn

and scalarize_fn untyped vecSig =
  let inTypes = Signature.input_types vecSig in
  let scalarSig = Signature.from_input_types (List.map Type.peel inTypes) in
  let scalarFn = specialize_value (SSA.GlobalFn untyped.fn_id) scalarSig in
  let scalarOutputTypes = scalarFn.fn_output_types in
  let outTypes = List.map (Type.increase_rank 1) scalarOutputTypes in
  let scalarClosure = mk_closure scalarFn [] in
  SSA_Codegen.mk_codegen_fn inTypes outTypes (fun codegen inputs outputs ->
    let outIds = List.map SSA_Helpers.get_id outputs in
    codegen#emit [
      mk_set outIds (mk_map scalarClosure inputs)
    ]
  )

and specialize_value  fnVal signature =
  IFDEF DEBUG THEN
    Printf.printf "Specialize_Value %s :: %s\n%!"
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
            if List.for_all Type.is_scalar inputTypes then
             mk_typed_scalar_prim op ?optOutType inputTypes
          else
            let nestedInputTypes = List.map Type.peel inputTypes in
            let nestedSig =
              match optOutType with
                | None -> Signature.from_input_types nestedInputTypes
                | Some outT ->
                  Signature.from_types nestedInputTypes [Type.peel outT]
            in
            let nestedFn = specialize_value fnVal nestedSig in
            mk_typed_map_fn nestedFn inputTypes
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

and specialize_fn_id fnId signature =
  IFDEF DEBUG THEN
    Printf.printf "Specialize_Function_Id...\n%!";
  ENDIF;
  specialize_value (GlobalFn fnId) signature
