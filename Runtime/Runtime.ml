(* pp: -parser o pa_macro.cmo *)

open Base
open Adverb
open PhiNode
open TypedSSA
open Value

type value = DataId.t Value.t
type values = value list

module type SCHEDULER = sig
  val call : TypedSSA.fn -> values -> values
  val adverb : (TypedSSA.fn, values, int list) Adverb.t -> values
  val array_op : Prim.array_op -> values -> values
end

module type INTERP = sig
  val eval_adverb : (TypedSSA.fn, values, int list) Adverb.t -> values
  val eval_call : TypedSSA.fn -> value list -> value list
  val eval_exp : TypedSSA.exp_node -> value list
end

module rec Scheduler : SCHEDULER = struct
  let machine_model = MachineModel.machine_model
  let value_to_host v = DataManager.to_memspace HostMemspace.id v

  let rec schedule_function fn args =
    (* for now, if we schedule a function which contains an adverb, *)
    (* never compile it but instead run the body in the interpreter *)
    let workTree = WorkTree.build_work_tree fn args in
    IFDEF DEBUG THEN WorkTree.to_str workTree; ENDIF;
    let plan = WorkTree.best_plan workTree in
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
    if (not hasAdverb) && shapely then (
      (* compile the function *)
      let results = LLVM_Backend.call fn (List.map value_to_host args) in
      List.map DataManager.from_memspace results
    )
    else Interp.eval_call fn args

  let call (fn:TypedSSA.fn) (args:values) =
    let results = schedule_function fn args in
    results

  let adverb (info:(TypedSSA.fn, values, int list) Adverb.t) =
    let fnTable = FnManager.get_typed_function_table() in 
    let _ = ShapeInference.infer_shape_env fnTable info.fn in 
    if ShapeInference.typed_fn_is_shapely info.fn then
      List.map DataManager.from_memspace (
        LLVM_Backend.adverb (
          Adverb.apply_to_fields
            info
            ~fn:Base.id
            ~values:(List.map value_to_host)
            ~axes:Base.id
        )
      )
    else begin
      IFDEF DEBUG THEN
        Printf.printf "[Scheduler] Running adverb %s in interpreter\n%!"
          (Adverb.adverb_type_to_str info.adverb_type)
      ENDIF; 
      Interp.eval_adverb info
    end 
  let array_op (op:Prim.array_op) (args:value list) : values =
    match op, args with
    | Prim.Range, [x] when Value.is_scalar x ->
      [Value.Range (0, Value.to_int x, 1)]
    | Prim.DimSize, [x; dim] ->
      let shape = Value.shape_of x in 
      [of_int $ Shape.get shape (Value.to_int dim)]

    | other, _ -> 
      failwith $ Printf.sprintf "Unsupport array operator %s"
      (Prim.array_op_to_str other) 
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

  type dir = Left | Right

  let eval_phi_node dir phiNode : unit =
    let id = phiNode.phi_id in
    let rhs = if dir = Left then phiNode.phi_left else phiNode.phi_right in
    let rhsVal = eval_value rhs in
    Env.set_binding id rhsVal

  let eval_phi_nodes dir phiNodes = List.iter (eval_phi_node dir) phiNodes

  let rec eval_block block = Block.iter_forward eval_stmt block
  and eval_stmt (stmtNode:TypedSSA.stmt_node) : unit =
    match stmtNode.stmt with
    | Set (ids, expNode) -> Env.set_bindings ids (eval_exp expNode)
    | SetIdx (id, indices, rhs) -> assert false
    | If (boolVal, tBlock, fBlock, phiNodes) ->
      let cond = Value.to_bool (eval_value boolVal) in
      eval_block (if cond then tBlock else fBlock);
      eval_phi_nodes (if cond then Left else Right) phiNodes
    | WhileLoop (testBlock, testVal, body, header) ->
      eval_phi_nodes Left header;
      eval_block testBlock;
      let cond = ref (eval_value testVal) in
      let niters = ref 0 in
      while Value.to_bool !cond do
        niters := !niters + 1;
        eval_block body;
        eval_phi_nodes Right header;
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
          ~axes:(fun xs -> List.map Value.to_int (eval_values xs))
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
    | Prim.Lt, [x;y] -> Value.of_bool (ParNum.to_float x < ParNum.to_float y)
    | Prim.Lte, [x;y] -> Value.of_bool (ParNum.to_float x <= ParNum.to_float y)
    | Prim.Gt, [x;y] -> Value.of_bool (ParNum.to_float x > ParNum.to_float y)
    | Prim.Gte, [x;y] -> Value.of_bool (ParNum.to_float x >= ParNum.to_float y)
    | Prim.Div, [x;y] -> Value.of_float (ParNum.to_float x /. ParNum.to_float y)
    (* other math operations should return the same type as their inputs, so *)
    (* use the generic math eval function                                    *)
    | op, _ -> Value.Scalar (MathEval.eval_pqnum_op op nums)

  let eval_map (fn:TypedSSA.fn) (fixed:values) (axes:int list) (args:values) =
    let shapes = List.map Value.shape_of args in
    let biggestShape =
      List.fold_left
        (fun acc s -> if Shape.rank s > Shape.rank acc then s else acc)
        Shape.scalar_shape
        shapes
    in
    let rec helper ?(axis_counter=0) args axes =
      match axes with
      | [] ->  eval_call fn (fixed@args)
      | axis::restAxes ->
        let nelts =  Shape.get biggestShape axis in
        (* create array of appropriate size, initialize to null *)
        let create_empty_array () = Array.create nelts Value.NoneVal in
        let results =
          List.map (fun _ -> create_empty_array()) fn.output_ids
        in
        let axisCounter' =
          if axis > axis_counter then axis_counter else axis_counter + 1
        in
        for i = 0 to nelts -1 do
          let slicedArgs = List.map (Value.fixdim (axis-axis_counter) i) args in
          let nestedResults =
            helper ~axis_counter:axisCounter' slicedArgs restAxes
          in
          List.iter2
            (fun result nested -> result.(i) <- nested)
            results
            nestedResults
        done;
        List.map (fun r -> Value.Tuple r) results
   in
   helper args axes

  let eval_adverb {adverb_type; fn; fixed; init; axes; args} =
    match adverb_type with
    | Adverb.Map -> eval_map fn fixed axes args
    | _ -> 
     failwith $ Printf.sprintf  
        "Adverb %s not implemented in interpreter"
        (Adverb.adverb_type_to_str adverb_type)
end

let call (fn:TypedSSA.fn) (hostData:Ptr.t Value.t list) : Ptr.t Value.t list =
  Env.push_env ();
  let inputs : values = List.map DataManager.from_memspace hostData in
  let outputVals : values = Scheduler.call fn inputs in
  let results = List.map (DataManager.to_memspace HostMemspace.id) outputVals in
  Env.pop_env ();
  results

let adverb info = 
  (* transform data from concrete host pointers to 
     abstract data ids 
  *) 
  let info' : (TypedSSA.fn, values, int list) Adverb.t = 
    Adverb.apply_to_fields 
      info
      ~fn:(fun (x:TypedSSA.fn) -> x)
      ~values:(List.map DataManager.from_memspace)
      ~axes:(fun (x:int list) -> x)
  in 
  Env.push_env(); 
  let results = 
    List.map (DataManager.to_memspace HostMemspace.id) $
      Scheduler.adverb info' 
  in 
  Env.pop_env(); 
  results 
  
