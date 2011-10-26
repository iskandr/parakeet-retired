(* pp: -parser o pa_macro.cmo *)

open Printf 
open Base

open SSA 
open Value 
 
type value = ArrayId.t Value.t  

let typeof v = DataManager.get_type v
let shapeof v = DataManager.get_shape v
let is_on_gpu v = DataManager.is_on_gpu v


let eval_value (valNode : SSA.value_node) : value =
  match valNode.value with
  | Var id -> DataManager.lookup id
  | Num n -> Value.Scalar n
  | _ ->
      let valStr = SSA.value_to_str valNode.value in
      failwith ("[eval_value] values of this type not implemented: " ^ valStr)
  
let eval_phi_node cond phiNode : unit = 
  let id = phiNode.phi_id in 
  let rhs = if cond then phiNode.phi_left else phiNode.phi_right in 
  let rhsVal = eval_value rhs in
  IFDEF DEBUG THEN 
    Printf.printf "\n===> [Eval] Phi assignment %s <- %s\n"
        (ID.to_str id)
        (Value.to_str rhsVal)
  ENDIF; 
  DataManager.set_binding id rhsVal 
    
let eval_phi_nodes cond phiNodes : unit =
  List.iter (eval_phi_node cond) phiNodes 
                        
let rec eval_block block = Block.iter_forward eval_stmt block 
and eval_stmt (stmtNode : SSA.stmt_node) : unit = 
  IFDEF DEBUG THEN 
    Printf.printf "\n===> EVAL: %s\n" (SSA.stmt_node_to_str stmtNode); 
  ENDIF; 
  match stmtNode.stmt with 
  | Set (ids, expNode) -> DataManager.set_bindings ids (eval_exp expNode) 
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
      cond := eval_value testVal; 
      IFDEF DEBUG THEN 
        Printf.printf "\n\n====> While Loop iteration %d\n%!" !niters; 
      ENDIF; 
    done
         
and eval_exp (expNode : SSA.exp_node) : value list = 
  match expNode.exp with 
  | Values valNodes -> List.map eval_value valNodes
  | Arr elts -> 
      [Value.Array (Array.of_list (List.map eval_value elts))]
  | Cast (t, valNode) when Type.is_scalar t -> 
      (match eval_value valNode with 
        | Value.Scalar n -> [Value.Scalar (ParNum.coerce_num n t)]
        | _ -> failwith "[eval] expected scalar"
      ) 
  | Cast (t, valNode) -> failwith "[eval] cast only implemented for scalars"
  
  (* first order array operators only *)
  | PrimApp (Prim.ArrayOp op, args) -> 
     let argVals = List.map eval_value args in
     eval_array_op op argVals expNode.exp_types 
        
  | PrimApp (Prim.ScalarOp op, args) -> 
      let argVals = List.map eval_value args in 
      [eval_scalar_op op argVals]
      
  | Call (fnId, args) -> 
      let argVals = List.map eval_value args in
      let fundef = get_fundef fnId in 
      eval_app fundef argVals
      
  (* currently ignores axes *)
  | Map ({ closure_fn = fnId; closure_args = closureArgs }, 
         {args=args; axes=axes}) ->
      let fn = FnManager.get_typed_function fnId in 
      let fixed = List.map eval_value closureArgs in
      let arrays = List.map eval_value args in  
      Scheduler.map fn ~axes ~fixed arrays
       
  | Reduce (reduceClosure, {args=args}) -> 
      let initFundef = get_fundef initClosure.closure_fn in
      let initClosureArgs = 
        List.map eval_value initClosure.closure_args 
      in
      let reduceFundef = get_fundef reduceClosure.closure_fn in
      let reduceClosureArgs = 
        List.map eval_value reduceClosure.closure_args 
      in 
      let initArgVals = List.map eval_value initArgs in 
      let argVals = List.map eval_value dataArgs in
        | Scan ({ closure_fn = initFnId; closure_args = initClosureArgs }, 
          { closure_fn = fnId; closure_args = closureArgs }, initArgs, args) -> 
     failwith "scan not implemented"
  | _ -> 
      failwith $ Printf.sprintf 
        "[eval_exp] no implementation for: %s\n"
        (SSA.exp_to_str expNode)
             
and eval_app fundef args =
  IFDEF DEBUG THEN 
  assert (List.length fundef.input_ids = List.length args); 
  ENDIF;
  DataManager.enter_scope (); 
  DataManager.set_bindings fundef.input_ids args;
  eval_block fundef.body;
  let outputs = List.map (DataManager.lookup P.memState) fundef.output_ids in 
  DataManager.exit_scope outputs; 
  outputs
    
and eval_scalar_op (op : Prim.scalar_op) (args : value list) =
(* whether the scalar is a GpuVal, a HostVal or an interpreter scalar, put *)
(* them all into hostvals                                                  *)
    let nums = List.map (DataManager.get_scalar P.memState) args in 
    match op, nums with 
    | Prim.Eq, [x; y] -> Value.of_bool (x = y) 
    | Prim.Neq, [x; y] -> Value.of_bool (x <> y) 
    | Prim.Lt, [x; y] ->
        Value.of_bool (ParNum.to_float x <= ParNum.to_float y) 
    | Prim.Lte, [x; y] -> 
        Value.of_bool (ParNum.to_float x <= ParNum.to_float y) 
    | Prim.Gt, [x; y] -> 
        Value.of_bool (ParNum.to_float x > ParNum.to_float y)
    | Prim.Gte, [x; y] -> 
        Value.of_bool (ParNum.to_float x >= ParNum.to_float y)
    | Prim.Div, [x; y] -> 
        Value.of_float (ParNum.to_float x /. ParNum.to_float y)
(* other math operations should return the same type as their inputs, so   *)
(* use the generic math eval function                                      *)
    | op, _ -> Value.Scalar (MathEval.eval_pqnum_op op nums)

and eval_array_op (op : Prim.array_op) (argVals : value list) : value list =
(* push a scope to clean up any non-escaping temporaries generated by      *)
(* array operators                                                         *)
    DataManager.enter_data_scope (); 
    let results = match op, argVals with
    | Prim.Index, [array; idx] ->
      (* always run on GPU *)
      let gpuArr = get_gpu array in 
      if Type.is_scalar (typeof idx) then (
        if Type.nest_depth (typeof array) = 1 then 
          let gpuVec = GpuVal.get_gpu_vec gpuArr in 
          let n = GpuVal.get_elt gpuVec (Value.to_int idx) in 
          [Value.Scalar n]
      (* TODO: What about pulling out whole rows from the GPU? *)
        else failwith "[Eval] Slicing a single row not yet supported" 
      )
      else 
        let gpuIdx = get_gpu idx in 
        let gpuResult = GpuEval.index gpuArr gpuIdx in 
        [add_gpu gpuResult] 
    | Prim.Where, [binVec] ->
      (* always run on GPU *)
      let gpuVec = get_gpu binVec in 
      let gpuResult = GpuEval.where gpuVec in 
      [add_gpu gpuResult] 
        
    | Prim.DimSize, [array; idx] ->
      (* always run on Host *)
      let s = DataManager.get_shape array in
      let i = Value.to_int idx in
      let result = Value.of_int (Shape.get s i) in 
      IFDEF DEBUG THEN 
        Printf.printf "DimSize(%s, %s): %s\n"
          (Value.to_str array)
          (Value.to_str idx)
          (Value.to_str result);
      ENDIF; 
      [result] 
    | Prim.Til, [n] ->
      (* always run on Host *)
       let count = ParNum.to_int (DataManager.get_scalar n) in
       let arr = Array.init count (fun i -> Value.of_int i) in 
       [Value.Array arr] 
    | Prim.Til, others -> 
      (* always run on host *)
      failwith $ Printf.sprintf 
        "Where's my scalar? I ordered a scalar: %s (host: %s)"
        (String.concat ", " (List.map Value.to_str others))
        (String.concat ", " 
          (List.map HostVal.to_str 
            (List.map (DataManager.get_host P.memState) others))) 

     | op, args -> 
        failwith $ Printf.sprintf "CPU operator not implemented: %s for args %s"
          (Prim.array_op_to_str op)
          (String.concat ", " (List.map Value.to_str args)) 
     in 
     DataManager.exit_data_scope results; 
     results 

let run fundef (hostData: Array.t Value.t list) =
  let vals = List.map (fun h -> DataManager.add_host memState h) hostData in 
  (* parameterize the evaluator by mutable global function table and       *)
	(* memory                                                                *)
  let outputVals = eval_app fundef vals in
  let results = List.map (DataManager.get_host memState) outputVals in 
  DataManager.flush_gpu memState; 
  results 