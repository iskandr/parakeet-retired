(* pp: -parser o pa_macro.cmo *)

open Base
open PhiNode
open TypedSSA
open Value

type value = DataId.t Value.t
type values = value list

module type SCHEDULER = sig
  val call : TypedSSA.fn -> values -> values
  val adverb : (TypedSSA.fn, values, values) Adverb.info -> values
  val array_op : Prim.array_op -> values -> values
end

module type INTERP = sig
  val eval_call : TypedSSA.fn -> value list -> value list
  val eval_exp : TypedSSA.exp_node -> value list
end

module rec Scheduler : SCHEDULER = struct
  let machine_model = MachineModel.machine_model
  let value_to_host v = DataManager.to_memspace HostMemspace.id v

  let call (fn:TypedSSA.fn) (args:values) =
    (* for now, if we schedule a function which contains an adverb, *)
    (* never compile it but instead run the body in the interpreter *)
    let hasAdverb = AdverbHelpers.fn_has_adverb fn in
    let shapely = ShapeInference.typed_fn_is_shapely fn in
    IFDEF DEBUG THEN
      Printf.printf
        "[Scheduler] Calling %s with args %s (has_adverb = %b, shapely=%b)\n%!"
        (FnId.to_str fn.TypedSSA.fn_id)
        (String.concat ", " (List.map Value.to_str args))
        hasAdverb
        shapely
      ;
    ENDIF;
    if not hasAdverb && shapely then
      (* compile the function *)
      let results = LLVM_Backend.call fn (List.map value_to_host args) in
      List.map DataManager.from_memspace results
    else Interp.eval_call fn args

  let adverb (info:(TypedSSA.fn, values, values) Adverb.info) =
    let results : Ptr.t Value.t list =
      LLVM_Backend.adverb $
        Adverb.apply_to_fields
          info
          ~fn:Base.id
          ~values:(List.map value_to_host)
          ~axes:(List.map Value.to_int)
    in
    List.map DataManager.from_memspace results

  let array_op (op:Prim.array_op) (args:value list) = assert false
end
and Interp : INTERP = struct
  let eval_value (valNode:TypedSSA.value_node) : value =
    match valNode.value with
    | Var id -> Env.lookup id
    | Num n -> Value.Scalar n
    | _ ->
      let valStr = TypedSSA.value_to_str valNode.value in
      failwith ("[eval_value] values of this type not implemented: " ^ valStr)

  let eval_values (valNodes:TypedSSA.value_nodes) : value list =
    List.map eval_value valNodes

  let eval_phi_node cond phiNode : unit =
    let id = phiNode.phi_id in
    let rhs = if cond then phiNode.phi_left else phiNode.phi_right in
    let rhsVal = eval_value rhs in
    Env.set_binding id rhsVal

  let eval_phi_nodes cond phiNodes = List.iter (eval_phi_node cond) phiNodes

  let rec eval_block block = Block.iter_forward eval_stmt block
  and eval_stmt (stmtNode:TypedSSA.stmt_node) : unit =
    match stmtNode.stmt with
    | Set (ids, expNode) -> Env.set_bindings ids (eval_exp expNode)
    | SetIdx (id, indices, rhs) -> assert false
    | If (boolVal, tBlock, fBlock, phiNodes) ->
      let cond = Value.to_bool (eval_value boolVal) in
      eval_block (if cond then tBlock else fBlock);
      eval_phi_nodes cond phiNodes
    | WhileLoop (testBlock, testVal, body, header) ->
      eval_phi_nodes true header;
      eval_block testBlock;
      let cond = ref (eval_value testVal) in
      let niters = ref 0 in
      while Value.to_bool !cond do
        niters := !niters + 1;
        eval_block body;
        eval_phi_nodes false header;
        eval_block testBlock;
        cond := eval_value testVal
      done

  and eval_exp (expNode:TypedSSA.exp_node) : value list =
    match expNode.exp with
    | Values valNodes -> eval_values valNodes
    | Arr elts ->
      let valArr = Array.of_list (eval_values elts) in
      let eltT = Type.elt_type (Value.type_of valArr.(0)) in
      let shape = Shape.of_list [Array.length valArr] in
      failwith "arrays not implemented"
      (*[Value.Array(valArr, eltT, shape)]*)
    | Cast (t, valNode) when Type.is_scalar t ->
      (match eval_value valNode with
        | Value.Scalar n -> [Value.Scalar (ParNum.coerce n (Type.elt_type t))]
        | _ -> failwith "[eval] expected scalar"
      )
    | Cast (t, valNode) -> failwith "[eval] cast only implemented for scalars"
    (* first order array operators only *)
    | PrimApp (Prim.ArrayOp op, args) ->
      Scheduler.array_op op (eval_values args)
    | PrimApp (Prim.ScalarOp op, args) ->
      [eval_scalar_op op (eval_values args)]
    | Call (fnId, args) ->
      let fn = FnManager.get_typed_function fnId in
      Scheduler.call fn (eval_values args)
    | Adverb adverbInfo ->
      Scheduler.adverb $
        Adverb.apply_to_fields
          adverbInfo
          ~fn:FnManager.get_typed_function
          ~values:eval_values
          ~axes:eval_values
    | _ ->
      failwith $ Printf.sprintf
        "[eval_exp] no implementation for: %s\n"
        (TypedSSA.exp_node_to_str expNode)

  and eval_call (fn:TypedSSA.fn) (args:values) =
    Env.push_env ();
    Env.set_bindings fn.input_ids args;
    eval_block fn.body;
    let outputs = List.map Env.lookup fn.output_ids in
    Env.pop_env ();
    outputs

  and eval_scalar_op (op:Prim.scalar_op) (args:value list) =
    (* whether the scalar is a GpuVal, a HostVal or an interpreter scalar, put *)
    (* them all into hostvals*)
    let nums = List.map Value.to_num args in
    match op, nums with
    | Prim.Eq, [x;y] -> Value.of_bool (x = y)
    | Prim.Neq, [x;y] -> Value.of_bool (x <> y)
    | Prim.Lt, [x;y] -> Value.of_bool (ParNum.to_float x <= ParNum.to_float y)
    | Prim.Lte, [x;y] -> Value.of_bool (ParNum.to_float x <= ParNum.to_float y)
    | Prim.Gt, [x;y] -> Value.of_bool (ParNum.to_float x > ParNum.to_float y)
    | Prim.Gte, [x;y] -> Value.of_bool (ParNum.to_float x >= ParNum.to_float y)
    | Prim.Div, [x;y] -> Value.of_float (ParNum.to_float x /. ParNum.to_float y)
    (* other math operations should return the same type as their inputs, so *)
    (* use the generic math eval function                                    *)
    | op, _ -> Value.Scalar (MathEval.eval_pqnum_op op nums)
end

let call (fn:TypedSSA.fn) (hostData:Ptr.t Value.t list) : Ptr.t Value.t list =
  Env.push_env ();
  let inputs : values = List.map DataManager.from_memspace hostData in
  let outputVals : values = Scheduler.call fn inputs in
  let results = List.map (DataManager.to_memspace HostMemspace.id) outputVals in
  Env.pop_env();
  results
