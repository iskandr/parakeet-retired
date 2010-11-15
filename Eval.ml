(* pp: -parser o pa_macro.cmo *)

open Printf 
open Base

open SSA 
open InterpVal 

let _ = Printexc.record_backtrace true 

module GpuCost = struct 
  let array_op memState fnTable op argVals = match op, argVals with 
  | Prim.Map, (InterpVal.Closure(fnId, []))::dataArgs -> 
    let launchCost = 1 in
    (* assume we can transfer 100,000 elements per millsecond to GPU, 
           and that allocation costs 3ms no matter the size 
         *)
    let memoryCosts = 
      List.sum (List.map (MemoryState.gpu_transfer_time memState) dataArgs)
    in  
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in  
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape
      | None -> failwith "no common shape found"
    in  
    (* assume each processor can process 1000 elements per millisecond, and 
       we have 100 processors
    *)
    let runCost = Shape.nelts maxShape / 100000  in 
    launchCost + memoryCosts + runCost 
  | _ -> 0         
end

module HostCost = struct 
  let array_op memState fnTable op argVals = match op, argVals with 
  | Prim.Map, (InterpVal.Closure(fnId, []))::dataArgs ->
    let memoryCosts = 
      List.sum (List.map (MemoryState.host_transfer_time memState) dataArgs)
    in
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in 
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape 
      | None -> failwith "max shape not found"
    in 
    (* assume we process 100 elements per millisecond on the host, 
       but also assume nested computations are free
     *) 
    let runCost = (Shape.get maxShape 0) / 100 in 
    2 + memoryCosts + runCost  
  | _ -> max_int (* don't run anything else on the host *)  
end


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
  | GlobalFn fnId -> InterpVal.Closure(fnId, [])
  | Stream (v,_) -> eval_value memoryState env v  
  | Str _
  | Sym _
  | Unit
  | Prim _
  | Lam _ -> failwith "[eval_value] values of this type not yet implemented" 
 
let rec eval globalFns fundef hostVals  =
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
  let outputVals = List.map (fun id -> ID.Map.find id env') fundef.output_ids
  in   
  let hostVals = List.map  (MemoryState.get_host memState) outputVals 
  in
  MemoryState.free_all_gpu memState;
  hostVals
  
and eval_block 
      (memState : MemoryState.t) 
      (fnTable : FnTable.t) 
      (env : env) : (SSA.stmt_node list -> env) = function  
  | [] -> env
  | stmtNode::rest ->
      IFDEF DEBUG THEN 
        Printf.sprintf "[eval_block] stmtNode::rest: %s \n%!"
             (SSA.stmt_node_to_str stmtNode);
      ENDIF; 
      let (env' : env) = eval_stmt memState fnTable env stmtNode in
      IFDEF DEBUG THEN 
        Printf.printf "[eval_block] evaluated stmt\n%!";
      ENDIF; 
      eval_block memState fnTable env' rest

and eval_stmt 
      (memState : MemoryState.t) 
      (fnTable : FnTable.t) 
      (env : env ) 
      (stmtNode : SSA.stmt_node) : env = match stmtNode.stmt with 
  | Set (ids, expNode) ->
      IFDEF DEBUG THEN
        Printf.printf "[eval_stmt] %s\n" (SSA.stmt_node_to_str stmtNode);
      ENDIF; 
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
  | Ignore expNode -> 
      ignore (eval_exp memState fnTable env expNode); env   
  | SetIdx (id, indices, rhs) -> failwith "not yet implemented"   
  | If (boolVal, tBlock, fBlock, ifGate) -> failwith "not yet implemented"

and eval_exp
      (memState : MemoryState.t) 
      (fnTable : FnTable.t)
      (env : env) 
      (expNode : SSA.exp_node) : InterpVal.t list = 
  match expNode.exp with 
  | Values valNodes -> List.map (eval_value memState env) valNodes         
  (* assume all array operators take only one function argument *) 
  | App ({value=Prim (Prim.ArrayOp op); value_type=ty}, args) ->
      let outTypes = DynType.fn_output_types ty in
      let argVals = List.map (eval_value memState env) args in
      eval_array_op memState fnTable env op argVals outTypes
  | App ({value=Prim (Prim.ScalarOp op)}, args) -> 
      let argVals = List.map (eval_value memState env) args in 
      eval_scalar_op memState op argVals   
  | App ({value=Var id}, args) ->
      failwith "calling closures not yet implemented"
  | App ({value=GlobalFn fnId}, args) -> 
      let fundef = FnTable.find fnId fnTable in
      let argVals = List.map (eval_value memState env) args in 
      eval_app memState fnTable env fundef argVals 
  | ArrayIndex (arr, indices) -> 
      failwith "[eval] array indexing not implemented" 
  | Arr elts -> failwith "[eval] array constructor not implemented"
  | Cast (t, valNode) when DynType.is_scalar t -> 
      (match eval_value  memState env valNode with 
        | InterpVal.Scalar n -> [InterpVal.Scalar (PQNum.coerce_num n t)]
        | _ -> failwith "[eval] expected scalar"
      )  
  | Cast (t, valNode) -> failwith "[eval] cast only implemented for scalars"
  | _ -> 
      failwith $ Printf.sprintf 
        "[eval_exp] no implementation for: %s\n"
        (SSA.exp_to_str expNode)
 

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
and eval_scalar_op memState args = failwith "not implemented"
and eval_array_op memState fnTable env op argVals outTypes : InterpVal.t list =
  let runOnGpu = GpuRuntime.implements_array_op op && 
    (let gpuCost = GpuCost.array_op memState fnTable op argVals in 
     let hostCost = HostCost.array_op memState fnTable op argVals in 
     Printf.printf 
      "Estimated cost of running array op %s on host: %d, on gpu: %d\n"
      (Prim.array_op_to_str op)
      hostCost
      gpuCost 
     ;
     gpuCost <= hostCost)
  in  
  if runOnGpu then 
    GpuRuntime.eval_array_op memState fnTable  op argVals outTypes 
  else match op, argVals with
  | Prim.Map, (InterpVal.Closure(fnId, [])::dataArgs) ->
      let fundef = FnTable.find fnId fnTable in
      eval_map memState fnTable env fundef dataArgs 
  | Prim.DimSize, [array; idx] -> 
      let s = MemoryState.get_shape memState array in
      let i = InterpVal.to_int idx in
      [InterpVal.of_int (Shape.get s i)] 
      
and eval_map memState fnTable env fundef argVals : InterpVal.t list =
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
  for i = 0 to n - 1 do
  let slices = 
    List.map 
      (fun v ->
         let t = MemoryState.get_type memState v in 
         if DynType.is_vec t then MemoryState.slice memState v i
         else v   
      ) 
      argVals
  in      
  let currResults = 
    Array.of_list (eval_app memState fnTable env fundef slices) 
  in
  for i = 0 to nOutputs - 1 do               
    let dynArr = allResults.(i) in
      DynArray.add dynArr currResults.(i)
    done             
  done;
  Array.to_list $ 
    Array.map 
      (fun dynArray -> InterpVal.Array (DynArray.to_array dynArray))
    allResults  
     