open Printf 
open Base

open SSA 
open InterpVal 

let _ = Printexc.record_backtrace true;;

type mem = MemoryState.mem_state
type env = (ID.t, InterpVal.t) PMap.t

(*
let get_fundef functions env fnValNode = match fnValNode.value with 
  | Lam fundef -> fundef 
  | GlobalFn fnId -> FnTable.find fnId functions    
  | Var id -> 
    let fnId = 
      if PMap.mem id env then match PMap.find id env with 
      | Closure (fnId, []) -> fnId
      | Closure (fnId, _) -> failwith "closures not yet supported" 
      | _ -> failwith "function expected"
      else id 
    in FnTable.find fnId functions    
  | _ -> failwith "function expected"
*)
    
let eval_value 
    (memoryState : mem) 
    (env : env) 
    (valNode : SSA.value_node) : InterpVal.t = 
  match valNode.value with 
  | Var id -> 
      if PMap.mem id env then PMap.find id env
      else failwith $ 
        Printf.sprintf "[eval_value] variable %s not found!" (ID.to_str id)
  | Num n -> MemoryState.add_host memoryState (HostVal.mk_host_scalar n)
  | GlobalFn fnId -> InterpVal.Closure(fnId, []) 
  | Str _
  | Sym _
  | Unit
  | Prim _
  | Lam _ -> failwith "[eval_value] values of this type not yet implemented" 
 
let rec eval globalFns fundef hostVals  =
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
    hostVals
  with exn ->
    MemoryState.free_all_gpu memState; raise exn

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
  | App ({value=Prim (Prim.ArrayOp op); value_type=ty}, args) ->
      let outTypes = DynType.fn_output_types ty in
      let argVals = List.map (eval_value memState env) args in 
      let gpuResults = 
        GpuRuntime.eval_array_op memState functions  op argVals outTypes 
      in 
      List.map (MemoryState.add_gpu memState) gpuResults 
  
  | App ({value=Var id}, args) ->
      failwith "calling closures not yet implemented"
  | App ({value=GlobalFn fnId}, args) -> 
      let fundef = FnTable.find fnId functions in
      debug (Printf.sprintf "[eval_exp] calling function %d" fnId);
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
       
  | _ -> failwith "[eval] eval_exp failed; node not implemented"
 
