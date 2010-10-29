open Printf 
open Base

open SSA 
open InterpVal 

let _ = Printexc.record_backtrace true;;

type mem = DataId.t MemoryState.mem_state
type env = (ID.t, InterpVal.t) PMap.t

let get_fundef functions env fnValNode = match fnValNode.value with 
  | Lam fundef -> fundef 
  | Var id -> 
    let fnid = 
      if PMap.mem id env then match PMap.find id env with 
      | Closure (fnid, []) -> fnid
      | Closure (fnid, _) -> failwith "closures not yet supported" 
      | _ -> failwith "function expected"
      else id 
    in FnTable.find fnid functions    
  | _ -> failwith "function expected"

    
let eval_value 
    (memoryState : mem) 
    (env : env) 
    (valNode : SSA.value_node) : InterpVal.t = 
  match valNode.value with 
  | Var id -> 
      if PMap.mem id env then PMap.find id env
      else 
        (* assume identifiers not in environment are global functions *)
        InterpVal.Closure (id, [])     
  | Num n -> MemoryState.add_host memoryState (HostVal.mk_host_scalar n) 
  | Str _
  | Sym _
  | Unit
  | Prim _
  | Lam _ -> failwith "[eval_value] values of this type not yet implemented" 
  
let eval_adverb_on_gpu 
      (memState : mem) 
      (functions : FnTable.t) 
      (env : env)
      (op : Prim.array_op) 
      (outputTypes : DynType.t list)
      (args : SSA.value_node list) : GpuVal.gpu_val list =
  let fnArgs, dataArgs = match op, args with
  | Prim.Map, fn::dataArgs -> [fn], dataArgs
  | Prim.Reduce, fn::dataArgs -> [fn], dataArgs
  | Prim.AllPairs, fn::dataArgs -> [fn], dataArgs
  | _ -> failwith "[eval_adverb_on_gpu] adverb not yet supported\n%!"
  in
  let inputTypes = List.map (fun vNode -> vNode.value_type) dataArgs in 
  let vals = List.map (eval_value memState env) dataArgs in  
  let fnIds = List.map SSA.get_id fnArgs in 
  let fundefs = List.map (fun id -> FnTable.find id functions) fnIds in  
  match op, fnIds, fundefs  with  
  | Prim.Map, [fnId], [fundef] ->
      GpuRuntime.run_map functions fnId inputTypes outputTypes memState vals 
  | Prim.Reduce, [fnId], [fundef] ->
      GpuRuntime.run_reduce functions fnId inputTypes outputTypes memState vals 
  | Prim.AllPairs, [fnId], [fundef] ->
      GpuRuntime.run_all_pairs 
        functions 
        fnId 
        inputTypes
        outputTypes
        memState
        vals 
  | _ -> failwith "This primitive not yet implemented on GPU"
 
let rec eval globalFns fundef hostVals  =
  GpuRuntime.init(); 
  let memState = MemoryState.create 127 (* arbitrary *) in
  (* create unique identifiers for data items *) 
  let vals = List.map (fun h -> MemoryState.add_host memState h) hostVals in 
  let (env : env) = List.fold_left2 
    (fun accEnv varId v -> PMap.add varId v accEnv) 
    PMap.empty
    fundef.input_ids 
    vals
  in  
  try 
    let env' = eval_block memState globalFns env fundef.body in 
    let outputVals = List.map (fun id -> PMap.find id env') fundef.output_ids
    in   
    let hostVals = List.map  (MemoryState.get_host memState) outputVals 
    in
    MemoryState.free_all_gpu memState; 
    GpuRuntime.shutdown(); 
    hostVals
  with exn ->
    MemoryState.free_all_gpu memState; GpuRuntime.shutdown(); raise exn

and eval_block 
      (memState : mem) 
      (functions : FnTable.t) 
      (env : env) : (SSA.stmt_node list -> env) = function  
  | [] -> env
  | stmtNode::rest ->
      debug (Printf.sprintf "[eval_block] stmtNode::rest: %s \n%!"
             (SSA.stmt_node_to_str stmtNode));
      let (env' : env) = eval_stmt memState functions env stmtNode in
      debug "[eval_block] evaluated stmt\n%!";
      eval_block memState functions env' rest

and eval_stmt 
      (memState : mem) 
      (functions : FnTable.t) 
      (env : env ) 
      (stmtNode : SSA.stmt_node) : env = match stmtNode.stmt with 
  | Set (ids, expNode) ->
      debug $ Printf.sprintf "[eval_stmt] %s" (SSA.stmt_node_to_str stmtNode);
      let results =  eval_exp memState functions env expNode in
      debug "[eval_stmt] after eval_exp";
      List.fold_left2 
        (fun accEnv id v -> PMap.add id v accEnv) 
        env 
        ids 
        results 
  | Ignore expNode -> 
      ignore (eval_exp memState functions env expNode); env   
  | SetIdx (id, indices, rhs) -> failwith "not yet implemented"   
  | If (boolVal, tBlock, fBlock, ifGate) -> failwith "not yet implemented"
and eval_exp
      (memState : mem) 
      (functions : FnTable.t) 
      (env : env) 
      (expNode : SSA.exp_node) : InterpVal.t list = 
  match expNode.exp with 
  | Values valNodes -> List.map (eval_value memState env) valNodes         
  (* assume all array operators take only one function argument *) 
  | App ({value=Prim (Prim.ArrayOp op); value_type=ty}, args) 
    when Prim.is_adverb op ->
      debug "[eval_exp] Prim.is_adverb\n%!";
      let outTypes = DynType.fn_output_types ty in 
      let gpuResults =
        eval_adverb_on_gpu memState functions env op outTypes args
      in 
      List.map (MemoryState.add_gpu memState) gpuResults 
      
  | App ({value=Prim _}, _) -> failwith "[eval] operator not yet implemented"
  | App ({value=Var id}, args) ->
      let fundef = FnTable.find id functions in
      debug (Printf.sprintf "[eval_exp] calling function %d \n " id);
      debug 
        (Printf.sprintf "[eval_exp] function: %s\n" (SSA.fundef_to_str fundef));
      let argVals = List.map (eval_value memState env) args in  
      (* create an augmented memory state where input ids are bound to the *)
      (* argument values on the gpu *) 
      let env' = 
        List.fold_left2 
          (fun accEnv id v -> PMap.add id v accEnv)
          env 
          fundef.input_ids
          argVals
      in
      let env'' = eval_block memState functions env' fundef.body in
      List.map (fun id -> PMap.find id env'') fundef.output_ids
      
  | ArrayIndex (arr, indices) -> 
        failwith "[eval] array indexing not implemented"
  | Arr elts -> failwith "[eval] array constructor not implemented"
  | Cast (t, valNode) -> failwith "[eval] cast not implemented"
  | _ -> failwith "[eval] eval_exp failed; unfound expNode.exp type"
 
