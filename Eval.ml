(* pp: -parser o pa_macro.cmo *)

open Printf 
open Base

open SSA 
open InterpVal 
 

type env = InterpVal.t ID.Map.t 

let _ = Printexc.record_backtrace true 

module type EVAL_PARAMS = sig 
  val fnTable : FnTable.t
  val memState : MemoryState.t 
end

module Mk(P : EVAL_PARAMS) = struct 
  module GpuEval = GpuRuntime.Mk(P) 
  let get_gpu : InterpVal.t -> GpuVal.gpu_val = 
    (MemoryState.get_gpu P.memState)
  let get_host : InterpVal.t -> HostVal.host_val = 
    (MemoryState.get_host P.memState)
  let add_gpu : GpuVal.gpu_val -> InterpVal.t = (MemoryState.add_gpu P.memState)  
  let add_host : HostVal.host_val -> InterpVal.t = 
    (MemoryState.add_host P.memState) 
  let get_fundef (fnId : FnId.t) : SSA.fundef = FnTable.find fnId P.fnTable  
    
  let rec eval_value (env : env) (valNode : SSA.value_node) : InterpVal.t = 
    match valNode.value with 
    | Var id -> 
      if ID.Map.mem id env then ID.Map.find id env
      else failwith $ 
        Printf.sprintf "[eval_value] variable %s not found!" (ID.to_str id)
    | Num n -> InterpVal.Scalar n 
    | _ -> 
      let valStr = SSA.value_to_str valNode.value in 
      failwith ("[eval_value] values of this type not implemented: " ^ valStr)
    
  and eval_block env  block = 
    let currEnv = ref env in 
    let n = SSA.block_length block in 
    for i = 0 to n- 1 do 
      currEnv := eval_stmt !currEnv (SSA.block_idx block i)
    done; 
    !currEnv 
  
  and eval_stmt (env : env) (stmtNode : SSA.stmt_node) : env = 
   IFDEF DEBUG THEN
        Printf.printf "[eval_stmt] %s\n" (SSA.stmt_node_to_str stmtNode);
  ENDIF;       
  match stmtNode.stmt with 
  | Set (ids, expNode) ->
      let results =  eval_exp env expNode in
      IFDEF DEBUG THEN
        debug "[eval_stmt] after eval_exp\n";
        assert (List.length ids = List.length results); 
      ENDIF; 
      ID.Map.extend env ids results 
  | SetIdx (id, indices, rhs) -> failwith "not yet implemented"   
  | If (boolVal, tBlock, fBlock, ifGate) -> failwith "not yet implemented"
  
and eval_exp (env : env) (expNode : SSA.exp_node) : InterpVal.t list = 
  match expNode.exp with 
  | Values valNodes -> List.map (eval_value env) valNodes
  | Arr elts -> failwith "[eval] array constructor not implemented"
  | Cast (t, valNode) when DynType.is_scalar t -> 
      (match eval_value  env valNode with 
        | InterpVal.Scalar n -> [InterpVal.Scalar (PQNum.coerce_num n t)]
        | _ -> failwith "[eval] expected scalar"
      )  
  | Cast (t, valNode) -> failwith "[eval] cast only implemented for scalars"
  
        
  (* first order array operators only *)          
  | PrimApp (Prim.ArrayOp op, args) -> 
     let argVals = List.map (eval_value env) args in
     eval_array_op env op argVals expNode.exp_types 
        
  | PrimApp (Prim.ScalarOp op, args) -> 
      let argVals = List.map (eval_value env) args in 
      eval_scalar_op op argVals
      
  | Call (fnId, args) -> 
      let argVals = List.map (eval_value env) args in
      let fundef = get_fundef fnId in 
      eval_app env fundef argVals
  
  | Map ({closure_fn=fnId; closure_args=closureArgs}, args) ->
      let fundef = get_fundef fnId in
      let closureArgVals : InterpVal.t list = 
        List.map (eval_value env) closureArgs 
      in 
      let argVals : InterpVal.t list = 
        List.map (eval_value env) args 
      in 
      let gpuCost = GpuCost.map P.memState closureArgVals argVals fundef in  
      let cpuCost = CpuCost.map P.memState closureArgVals argVals fundef in
      (if gpuCost < cpuCost then 
        let gpuResults = 
          GpuEval.map 
            ~payload:fundef  
            ~closureArgs:(List.map get_gpu closureArgVals) 
            ~args:(List.map get_gpu argVals)
        in 
        List.map add_gpu gpuResults
      else 
        eval_map env ~payload:fundef closureArgVals argVals
      )
      
  | Reduce (initClosure, reduceClosure, dataArgs)-> 
      let initFundef = get_fundef initClosure.closure_fn in
      (* the current reduce kernel works either for 1d data or 
         for maps over 2d data 
      *) 
      let initFundef =  match SSA.extract_nested_map_fn_id initFundef with  
        | Some nestedFnId -> get_fundef nestedFnId 
        | None -> initFundef 
      in 
      let initClosureArgs = 
        List.map (eval_value env) initClosure.closure_args 
      in
       
      let reduceFundef = get_fundef reduceClosure.closure_fn in
      let reduceFundef = match SSA.extract_nested_map_fn_id reduceFundef with 
        | Some nestedFnId -> get_fundef nestedFnId 
        | None -> reduceFundef 
      in  
      let reduceClosureArgs = 
        List.map (eval_value env) reduceClosure.closure_args 
      in 
      let argVals  = List.map (eval_value env) dataArgs in
      let gpuCost = 0 
        (* 
        GpuCost.reduce ~memState:P.memState ~init:initFundef ~initClosureArgs 
          ~fn:reduceFundef ~closureArgs:reduceClosureArgs ~argss:argVals
        *)
      in    
      let cpuCost = 100 in 
 
        (*CpuCost.map P.memState closureArgVals argVals fundef in*) 
      (if gpuCost < cpuCost then
        let gpuResults = 
          GpuEval.reduce
            ~init:initFundef
            ~initClosureArgs:(List.map get_gpu initClosureArgs)
            ~payload:reduceFundef 
            ~payloadClosureArgs:(List.map get_gpu reduceClosureArgs)
            ~args:(List.map get_gpu argVals) 
        in 
        List.map add_gpu gpuResults
      else 
        failwith "cpu map not implemented"
      ) 
      
  | Scan ({closure_fn=initFnId; closure_args=initClosureArgs}, 
          {closure_fn=fnId; closure_args=closureArgs}, args) ->    
     failwith "scan not implemented"
  | _ -> 
      failwith $ Printf.sprintf 
        "[eval_exp] no implementation for: %s\n"
        (SSA.exp_to_str expNode)
             
  and eval_app env fundef args = 
  (* create an augmented memory state where input ids are bound to the *)
  (* argument values on the gpu *) 
    let env2 = ID.Map.extend env fundef.input_ids args in  
    let env3 = eval_block env2 fundef.body in
    List.map (fun id -> ID.Map.find id env3) fundef.output_ids
  and eval_scalar_op args = failwith "scalar op not implemented"
  and eval_array_op env op argVals outTypes : InterpVal.t list =
    let gpuCost = GpuCost.array_op P.memState op argVals in 
    let hostCost = CpuCost.array_op P.memState op argVals in
    IFDEF DEBUG THEN 
       Printf.printf 
         "Estimated cost of running array op %s on host: %d, on gpu: %d\n"
         (Prim.array_op_to_str op)
         hostCost
         gpuCost;
    ENDIF;   
    let runOnGpu = gpuCost <= hostCost in   
    match op, argVals with
    | Prim.Index, [array; idx] -> 
      (* always run on GPU *)
      let gpuArr = get_gpu array in 
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
      let s = MemoryState.get_shape P.memState array in
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
       (* always run on Host *)
       let count =  PQNum.to_int (MemoryState.get_scalar P.memState n) in
       let arr = Array.init count (fun i -> InterpVal.of_int i) in 
       [InterpVal.Array arr] 
    | Prim.Til, others -> 
      (* always run on host *) 
      failwith $ Printf.sprintf 
        "You ain't no scalar. Where's my scalar? I ordered a scalar: %s (host: %s)"
        (String.concat ", " (List.map InterpVal.to_str others))
        (String.concat ", " 
          (List.map HostVal.to_str 
            (List.map (MemoryState.get_host P.memState) others))) 

           
  and eval_map env ~payload closureArgs argVals =
    Printf.printf "Running MAP on host!\n";
    let dataShapes = List.map (MemoryState.get_shape P.memState) argVals in
    let maxShape = match Shape.max_shape_list dataShapes with 
    | Some maxShape -> maxShape 
    | None -> 
      failwith "Incompatible array shapes encountered while evaluating Map"
    in 
    (* if trying to map over scalars, just evaluate this function directly *)  
    if Shape.rank maxShape = 0 then eval_app env payload argVals    
    else 
    let n = Shape.get maxShape 0 in
    let outputIds = Array.of_list (payload.output_ids) in
    let nOutputs = Array.length outputIds in  
    let allResults = Array.init nOutputs  (fun _ -> DynArray.create ()) in
    let get_slice idx v =
      Printf.printf "Getting slice %d\n%!" idx;  
      let t = MemoryState.get_type P.memState v in 
      if DynType.is_vec t then MemoryState.slice P.memState v idx
      else v    
    in 
    for elt = 0 to n - 1 do
      Printf.printf "Iteration %d\n" elt; 
      let slices = List.map (get_slice elt) argVals in
      Printf.printf "Got slice!\n%!";  
      let inputs = (closureArgs @ slices) in
      let currResults = 
        Array.of_list (eval_app env payload inputs)
      in 
      for i = 0 to nOutputs - 1 do 
        DynArray.add allResults.(i) currResults.(i)
      done;    
    done;   
    let mk_array dynArray = InterpVal.Array (DynArray.to_array dynArray) in  
    Array.to_list $ Array.map mk_array  allResults  
end 

 
let eval globalFns fundef hostVals =
  let memState = MemoryState.create 127 (* arbitrary *) in  
  (* create unique identifiers for data items *) 
  let vals = List.map (fun h -> MemoryState.add_host memState h) hostVals in
  IFDEF DEBUG THEN 
    assert (List.length fundef.input_ids = List.length vals); 
  ENDIF;  
  let (env : env) = ID.Map.extend ID.Map.empty fundef.input_ids vals in
  (* parameterize the evaluator by mutable global function table and memory *)
  let module E = 
    Mk(struct let fnTable = globalFns let memState = memState end) 
  in  
  let env' = E.eval_block env fundef.body in
  let outputVals = 
    List.map (fun id -> ID.Map.find id env') fundef.output_ids
  in
  let hostVals = List.map (MemoryState.get_host memState) outputVals in
  MemoryState.free_all_gpu memState;
  hostVals

(*
module type INTERP = sig 
  type source_info (* attach this as the type param of SourceInfo.t *) 
  val eval : InterpState.t -> MemoryState.t -> env ->  SSA.stmt_node -> env    
end

type 'a closure = SSA.fundef * 'a 

module type BACKEND = sig 
  type data 
  
  val to_device : HostVal.host_val -> data 
  val from_device : data -> HostVal.host_val 
  
  val map : data closure -> data list -> data list 
  val reduce : data closure -> data closure -> data list -> data list
  val scan : data closure -> data closure -> data list -> data list
  
  val array_op : Prim.array_op -> data list -> data list     
end
*)