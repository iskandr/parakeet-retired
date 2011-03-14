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
  (* pass the global fnTable, memState to modules we want to use *)  
  module GpuEval = GpuRuntime.Mk(P)
   
  let get_gpu interpVal  = MemoryState.get_gpu P.memState interpVal 
  let get_host interpVal =  MemoryState.get_host P.memState interpVal 
  let add_gpu gpuVal= MemoryState.add_gpu P.memState gpuVal   
  let add_host hostVal =  MemoryState.add_host P.memState hostVal 
  let get_fundef (fnId : FnId.t) : SSA.fundef = FnTable.find fnId P.fnTable
    
  let typeof interpVal = MemoryState.get_type P.memState interpVal 
  let shapeof interpVal = MemoryState.get_shape P.memState interpVal
  let is_on_gpu interpVal = MemoryState.is_on_gpu P.memState interpVal
  
  (* the cost model function expect arguments to be described by triplets of *)
  (* their type, shape, and a boolean indicating whether that argument is on*)
  (* the gpu. *)
  let describe_arg interpVal = 
    typeof interpVal, shapeof interpVal, is_on_gpu interpVal 
  let describe_args interpVals = List.map describe_arg interpVals
       
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
    
  and eval_block env block = Block.fold_forward eval_stmt env block  
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
    | SetIdx (id, indices, rhs) -> assert false    
    | If (boolVal, tBlock, fBlock, ifGate) -> 
        assert false 
    | WhileLoop (testBlock, testVal, body, header, exit) -> assert false
       (*
      let env' = eval_block env  in 
      (match eval_value env condVal with 
        | InterpVal.Scalar (PQNum.Bool true) -> 
            let env' = eval_block env body in 
            eval_stmt env' stmtNode (* loop implemented via recursive call *)    
        | InterpVal.Scalar (PQNum.Bool false) -> 
            env (* not handling SSA gate properly *)  
        | _ -> failwith "expected boolean value for loop condition" 
      ) *)
         
and eval_exp (env : env) (expNode : SSA.exp_node) : InterpVal.t list = 
  match expNode.exp with 
  | Values valNodes -> List.map (eval_value env) valNodes
  | Arr elts -> 
      [InterpVal.Array (Array.of_list (List.map (eval_value env) elts))]
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
      let argVals : InterpVal.t list = List.map (eval_value env) args in
      IFDEF DEBUG THEN
        Printf.printf "args to map: %s\n"
          (String.concat ", " (List.map InterpVal.to_str argVals)); 
      ENDIF;  
      let closureArgInfo = describe_args closureArgVals in 
      let argInfo = describe_args argVals in
      let bestLoc, bestTime = 
        CostModel.map_cost P.fnTable fundef closureArgInfo argInfo 
      in 
      begin match bestLoc with  
        | CostModel.GPU ->
            let gpuResults = 
              GpuEval.map 
                ~payload:fundef  
                ~closureArgs:(List.map get_gpu closureArgVals) 
                ~args:(List.map get_gpu argVals)
            in List.map add_gpu gpuResults
        | CostModel.CPU -> eval_map env ~payload:fundef closureArgVals argVals   
      end
  | Reduce (initClosure, reduceClosure, initArgs, dataArgs)-> 
      let initFundef = get_fundef initClosure.closure_fn in
      (* the current reduce kernel works either for 1d data or 
         for maps over 2d data 
      *) 
       
      let initClosureArgs = 
        List.map (eval_value env) initClosure.closure_args 
      in
      let reduceFundef = get_fundef reduceClosure.closure_fn in
      let reduceClosureArgs = 
        List.map (eval_value env) reduceClosure.closure_args 
      in 
      let initArgVals = List.map (eval_value env) initArgs in 
      let argVals  = List.map (eval_value env) dataArgs in
      let gpuCost = 0 in   
      let cpuCost = 100000 in  
      let reduceFundef = match SSA.extract_nested_map_fn_id reduceFundef with 
        | Some nestedFnId -> get_fundef nestedFnId 
        | None -> reduceFundef 
      in
      (if gpuCost < cpuCost then
        let gpuResults = 
          GpuEval.reduce
            ~init:initFundef
            ~initClosureArgs:(List.map get_gpu initClosureArgs)
            ~payload:reduceFundef 
            ~payloadClosureArgs:(List.map get_gpu reduceClosureArgs)
            ~initArgs:(List.map get_gpu initArgVals)
            ~args:(List.map get_gpu argVals) 
        in 
        List.map add_gpu gpuResults
      else 
        failwith "cpu map not implemented"
      ) 
      
  | Scan ({closure_fn=initFnId; closure_args=initClosureArgs}, 
          {closure_fn=fnId; closure_args=closureArgs}, initArgs, args) ->    
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
    let bestLoc, bestCost = CostModel.array_op op (describe_args argVals) in 
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
    IFDEF DEBUG THEN Printf.printf "Running MAP on host!\n"; ENDIF; 
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
      IFDEF DEBUG THEN 
        Printf.printf "Getting slice %d\n%!" idx;
      ENDIF;   
      let t = MemoryState.get_type P.memState v in 
      if DynType.is_vec t then MemoryState.slice P.memState v idx
      else v    
    in 
    for elt = 0 to n - 1 do
      let slices = List.map (get_slice elt) argVals in
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

