(* pp: -parser o pa_macro.cmo *)

open Printf 
open Base

open SSA 
open InterpVal 

let _ = Printexc.record_backtrace true 

type env = InterpVal.t ID.Map.t 

let rec eval_value 
    (memoryState : MemoryState.t) 
    (env : env) 
    (valNode : SSA.value_node) : InterpVal.t = 
  match valNode.value with 
  | Var id -> 
      if ID.Map.mem id env then ID.Map.find id env
      else failwith $ 
        Printf.sprintf "[eval_value] variable %s not found!" (ID.to_str id)
  | Num n -> InterpVal.Scalar n 
        (*MemoryState.add_host memoryState (HostVal.mk_host_scalar n)*)
 
  | _ -> failwith ("[eval_value] values of this type not implemented: " ^
           (SSA.value_to_str valNode.value))
 
let rec eval globalFns fundef hostVals =
  let memState = MemoryState.create 127 (* arbitrary *) in
  (* create unique identifiers for data items *) 
  let vals = List.map (fun h -> MemoryState.add_host memState h) hostVals in
  IFDEF DEBUG THEN 
    assert (List.length fundef.input_ids = List.length vals); 
  ENDIF;  
  let (env : env) = List.fold_left2 
    (fun accEnv varId v -> ID.Map.add varId v accEnv) 
    ID.Map.empty
    fundef.input_ids 
    vals
  in
  let env' = eval_block memState globalFns env fundef.body in
  IFDEF DEBUG THEN 
    Printf.printf "done evaluating body\n%!"; 
  ENDIF;  
  let outputVals = List.map (fun id -> ID.Map.find id env') fundef.output_ids
  in
  IFDEF DEBUG THEN 
    Printf.printf "getting variables on host\n%!"; 
  ENDIF; 
  let hostVals = List.map (MemoryState.get_host memState) outputVals in
  IFDEF DEBUG THEN 
     Printf.printf "free_all\n%!";
  ENDIF; 
  MemoryState.free_all_gpu memState;
  IFDEF DEBUG THEN 
     Printf.printf "returning\n%!";
  ENDIF; 
  hostVals
  
and eval_block (memState : MemoryState.t) (fnTable : FnTable.t) env  block = 
  let currEnv = ref env in 
  let n = SSA.block_length block in 
  for i = 0 to n- 1 do 
    currEnv := eval_stmt memState fnTable !currEnv (SSA.block_idx block i)
  done; 
  !currEnv 
  
and eval_stmt 
      (memState : MemoryState.t) 
      (fnTable : FnTable.t) 
      (env : env ) 
      (stmtNode : SSA.stmt_node) : env = 
   IFDEF DEBUG THEN
        Printf.printf "[eval_stmt] %s\n" (SSA.stmt_node_to_str stmtNode);
  ENDIF;       
  match stmtNode.stmt with 
  | Set (ids, expNode) ->
      let results =  eval_exp memState fnTable env expNode in
      IFDEF DEBUG THEN
        debug "[eval_stmt] after eval_exp\n";
        assert (List.length ids = List.length results); 
      ENDIF; 
      List.fold_left2 
        (fun accEnv id v -> ID.Map.add id v accEnv) 
        env 
        ids 
        results 
  | SetIdx (id, indices, rhs) -> failwith "not yet implemented"   
  | If (boolVal, tBlock, fBlock, ifGate) -> failwith "not yet implemented"

and eval_exp
      (memState : MemoryState.t) 
      (fnTable : FnTable.t)
      (env : env) 
      (expNode : SSA.exp_node) : InterpVal.t list = 
  match expNode.exp with 
  | Values valNodes -> List.map (eval_value memState env) valNodes
  | Arr elts -> failwith "[eval] array constructor not implemented"
  | Cast (t, valNode) when DynType.is_scalar t -> 
      (match eval_value  memState env valNode with 
        | InterpVal.Scalar n -> [InterpVal.Scalar (PQNum.coerce_num n t)]
        | _ -> failwith "[eval] expected scalar"
      )  
  | Cast (t, valNode) -> failwith "[eval] cast only implemented for scalars"
  
        
  (* first order array operators only *)          
  | PrimApp ({prim=Prim.ArrayOp op}, args) -> 
     let argVals = List.map (eval_value memState env) args in
     eval_array_op memState fnTable env op argVals expNode.exp_types 
        
  | PrimApp ({prim=Prim.ScalarOp op}, args) -> 
      let argVals = List.map (eval_value memState env) args in 
      eval_scalar_op memState op argVals
      
  | Call (fnId, args) -> 
      let argVals = List.map (eval_value memState env) args in
      let fundef = FnTable.find fnId fnTable in 
      eval_app memState fnTable env fundef argVals
  
  | Map ({closure_fn=fnId; closure_args=closureArgs}, args) ->
      let fundef = FnTable.find fnId fnTable in
      let closureArgVals : InterpVal.t list = 
        List.map (eval_value memState env) closureArgs 
      in 
      let argVals : InterpVal.t list = 
        List.map (eval_value memState env) args 
      in 
      let gpuCost : int = GpuCost.map memState closureArgVals argVals fundef in  
      let cpuCost : int = CpuCost.map memState closureArgVals argVals fundef in
      (if gpuCost < cpuCost then 
        GpuRuntime.eval_map 
          memState 
          fnTable 
          fundef 
          closureArgVals 
          argVals 
          expNode.exp_types
      else 
        failwith "cpu map not implemented"
      )
  | _ -> 
      failwith $ Printf.sprintf 
        "[eval_exp] no implementation for: %s\n"
        (SSA.exp_to_str expNode)           
      (*
  | Reduce (
     {closure_fn=initFnId; closure_args=initClosureArgs},
     {closure_fn=reduceFnId; closure_args=reduceClosureArgs},
     dataArgs
    ) -> 
      let fundef = FnTable.find reduceFnId fnTable in
      (* the current reduce kernel works either for 1d data or 
         for maps over 2d data 
      *) 
      let fundef2 = match SSA.extract_nested_map_fn_id fundef with 
        | Some nestedFnId -> FnTable.find nestedFnId fnTable
        | None -> fundef 
      in  
      
      let gpuVals = List.map (MemoryState.get_gpu memState) dataArgs in
      run_reduce memState fnTable fundef2 gpuVals outputTypes
      *)
  | Scan ({closure_fn=initFnId; closure_args=initClosureArgs}, 
          {closure_fn=fnId; closure_args=closureArgs}, args) ->    
     failwith "scan not implemented"
and eval_app memState fnTable env fundef args = 
  (* create an augmented memory state where input ids are bound to the *)
  (* argument values on the gpu *) 
  let env2 = 
    List.fold_left2 
      (fun accEnv id v -> ID.Map.add id v accEnv)
      env 
      fundef.input_ids
      args
  in
  let env3 = eval_block memState fnTable env2 fundef.body in
  List.map (fun id -> ID.Map.find id env3) fundef.output_ids
and eval_scalar_op memState args = failwith "scalar op not implemented"
and eval_array_op memState fnTable env op argVals outTypes : InterpVal.t list =
  let gpuCost = GpuCost.array_op memState op argVals in 
  let hostCost = CpuCost.array_op memState op argVals in
  IFDEF DEBUG THEN 
       Printf.printf 
         "Estimated cost of running array op %s on host: %d, on gpu: %d\n"
         (Prim.array_op_to_str op)
         hostCost
         gpuCost;
  ENDIF;   
  let runOnGpu = GpuRuntime.implements_array_op op && gpuCost <= hostCost in  
  if runOnGpu then
    GpuRuntime.eval_array_op memState fnTable  op argVals outTypes
  else match op, argVals with
  (*| Prim.Map, (InterpVal.Closure(fnId, closureArgs)::dataArgs) ->
      let fundef = FnTable.find fnId fnTable in
      eval_map memState fnTable env fundef closureArgs dataArgs
   *) 
  | Prim.DimSize, [array; idx] -> 
      let s = MemoryState.get_shape memState array in
      let i = InterpVal.to_int idx in
      let result = InterpVal.of_int (Shape.get s i) in 
      IFDEF DEBUG THEN 
        Printf.printf "DimSize(%s, %s): %s\n"
          (InterpVal.to_str array)
          (InterpVal.to_str idx)
          (InterpVal.to_str result);
      ENDIF;  
      [result]   
  | Prim.Til, [n] ->
       let count =  PQNum.to_int (MemoryState.get_scalar memState n) in
       let arr = Array.init count (fun i -> InterpVal.of_int i) in 
       [InterpVal.Array arr] 
  | Prim.Til, others -> 
      failwith $ Printf.sprintf 
        "You ain't no scalar. Where's my scalar? I ordered a scalar: %s (host: %s)"
        (String.concat ", " (List.map InterpVal.to_str others))
        (String.concat ", " 
          (List.map HostVal.to_str 
            (List.map (MemoryState.get_host memState) others))) 

          
and eval_map memState fnTable env fundef closureArgs argVals =
  Printf.printf "Running MAP on host!\n";
  let dataShapes = List.map (MemoryState.get_shape memState) argVals in
  let maxShape = match Shape.max_shape_list dataShapes with 
  | Some maxShape -> maxShape 
  | None -> 
      failwith "Incompatible array shapes encountered while evaluating Map"
  in 
  (* if trying to map over scalars, just evaluate this function directly *)  
  if Shape.rank maxShape = 0 then 
    eval_app memState fnTable env fundef argVals    
  else 
  let n = Shape.get maxShape 0 in
  let outputIds = Array.of_list (fundef.output_ids) in
  let nOutputs = Array.length outputIds in  
  let allResults = Array.init nOutputs  (fun _ -> DynArray.create ()) in
  let get_slice idx v =
    Printf.printf "Getting slice %d\n%!" idx;  
    let t = MemoryState.get_type memState v in 
    if DynType.is_vec t then MemoryState.slice memState v idx
    else v    
  in 
  for elt = 0 to n - 1 do
    Printf.printf "Iteration %d\n" elt; 
    let slices = List.map (get_slice elt) argVals in
    Printf.printf "GOt slice!\n%!";  
    let inputs = (closureArgs @ slices) in
    let currResults = 
      Array.of_list (eval_app memState fnTable env fundef inputs)
    in 
    for i = 0 to nOutputs - 1 do 
      DynArray.add allResults.(i) currResults.(i)
    done;    
  done;   
  Array.to_list $
    Array.map
      (fun dynArray -> Printf.printf "Creating DynArray\n%!"; InterpVal.Array (DynArray.to_array dynArray))
    allResults  
