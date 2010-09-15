open Printf 
open Base

open SSA 
open ImpGenReduce
open ImpGenMap
open ImpGenAllPairs
open ImpToPtx
open DynVal

type mem = DataId.t MemoryState.mem_state
type env = (ID.t, DynVal.dyn_val) PMap.t
type fntable = (ID.t, SSA.fundef) Hashtbl.t
type codekey = SSA.value * Signature.t 
type codecache = (codekey, Cuda.compiled_kernel) Hashtbl.t 

let get_fundef functions env fnValNode = match fnValNode.value with 
  | Lam fundef -> fundef 
  | Var id -> 
    let fnid = 
      if PMap.mem id env then match PMap.find id env with 
      | DynVal.FnRef fnid -> fnid 
      | _ -> failwith "function expected"
      else id 
    in Hashtbl.find functions fnid   
  | _ -> failwith "function expected"

    
let eval_value 
    (env : env) 
    (valNode : SSA.value_node) : DynVal.dyn_val = 
  match valNode.value with 
  | Var id -> 
      if PMap.mem id env then PMap.find id env
      else 
        (* assume identifiers not in environment are global functions *)
        DynVal.FnRef id     
  | Num n -> DynVal.Scalar n  
  | Str _
  | Sym _
  | Unit
  | Prim _
  | Lam _ -> failwith "[eval] values of this type not yet implemented on gpu" 
  
let eval_adverb_on_gpu 
      (codeCache : codecache)
      (memState : mem) 
      (functions : fntable) 
      (env : env)
      (op : Prim.array_op) 
      (outputTypes : DynType.t list)
      (args : SSA.value_node list) : GpuVal.gpu_val list =
  let fnArgs, dataArgs = match op, args with 
  | Prim.Map, fn::dataArgs -> [fn], dataArgs
  | Prim.AllPairs, fn::dataArgs -> [fn], dataArgs
  in
  let signature = { 
    Signature.inputs = 
      (List.map (fun v -> Signature.Value v.value) fnArgs) @ 
      (List.map (fun v -> Signature.Type v.value_type) dataArgs);
    outputs = Some outputTypes
  }
  in 
  let key = Prim (Prim.ArrayOp op), signature in
  let fundefs = List.map (get_fundef functions env) fnArgs in
  let inputTypes = List.map (fun v -> v.value_type) dataArgs in 
    
  let compiledModule = 
    if Hashtbl.mem codeCache key then Hashtbl.find codeCache key 
    else 
      let compiledModule = match op, fundefs, outputTypes with 
      | Prim.Map, [fundef], _ -> 
          GpuRuntime.compile_map fundef inputTypes outputTypes   
      | Prim.AllPairs, [fundef], [outputType] ->
        GpuRuntime.compile_all_pairs fundef inputTypes outputType 
      | _ -> failwith "compilation for this primitive not yet implemented"
      in (Hashtbl.add codeCache key compiledModule; compiledModule)
  in 
  let dynVals = List.map (eval_value env) dataArgs in
  let gpuVals = List.map (MemoryState.get_gpu memState) dynVals in  
  match op with 
    | Prim.Map -> GpuRuntime.run_map compiledModule gpuVals outputTypes   
    | _ -> failwith "execution for this primitive not yet implemented"    



                                             
let rec eval codeCache globalFns fundef hostVals  =
  GpuRuntime.init(); 
  let memState = MemoryState.create 127 (* arbitrary *) in
  (* create unique identifiers for data items *) 
  let dynvals = List.map (fun h -> MemoryState.add_host memState h) hostVals in 
  let (env : env) = List.fold_left2 
    (fun accEnv varId dynval -> PMap.add varId dynval accEnv) 
    PMap.empty
    fundef.input_ids 
    dynvals
  in  
  try 
    let env' = eval_block codeCache memState globalFns env fundef.body in 
    let outputDynVals = List.map (fun id -> PMap.find id env') fundef.output_ids
    in   
    let hostVals = List.map  (MemoryState.get_host memState) outputDynVals 
    in
    MemoryState.free_all_gpu memState; 
    GpuRuntime.shutdown(); 
    hostVals
  with exn ->
    MemoryState.free_all_gpu memState; GpuRuntime.shutdown(); raise exn

and eval_block 
      (codeCache : codecache) 
      (memState : mem) 
      (functions : fntable) 
      (env : env) : (SSA.stmt_node list -> env) = function  
  | [] -> env
  | stmtNode::rest -> 
      let (env' : env) = eval_stmt codeCache memState functions env stmtNode  in 
      eval_block codeCache memState functions env' rest

and eval_stmt 
      (codeCache : codecache)
      (memState : mem) 
      (functions : fntable) 
      (env : env ) 
      (stmtNode : SSA.stmt_node) : env = match stmtNode.stmt with 
  | Set (ids, expNode) -> 
      let results =  eval_exp codeCache memState functions env expNode in 
      List.fold_left2 
        (fun accEnv id dynval -> PMap.add id dynval accEnv) 
        env 
        ids 
        results 
  | Ignore expNode -> 
      ignore (eval_exp codeCache memState functions env expNode); env   
  | SetIdx (id, indices, rhs) -> failwith "not yet implemented"   
  | If (boolVal, tBlock, fBlock, ifGate) -> failwith "not yet implemented"    
and eval_exp
      (codeCache : codecache)
      (memState : mem) 
      (functions : fntable) 
      (env : env) 
      (expNode : SSA.exp_node) : DynVal.dyn_val list = 
  match expNode.exp with 
  | Values valNodes -> List.map (eval_value env) valNodes         
  (* assume all array operators take only one function argument *) 
  | App ({value=Prim (Prim.ArrayOp op); value_type=ty}, args) 
    when Prim.is_adverb op -> 
      let gpuResults = 
        eval_adverb_on_gpu codeCache memState functions env 
        op (DynType.fn_output_types ty) args
      in 
      List.map (MemoryState.add_gpu memState) gpuResults 
      
  | App ({value=Prim _}, _) -> failwith "[eval] operator not yet implemented"
  | App ({value=Var id}, args) ->
      let fundef = Hashtbl.find functions id in
      printf "[eval_exp] calling function %d \n " id;
      printf "[eval_exp] function: %s\n" (SSA.fundef_to_str fundef);
      let argDynVals = List.map (eval_value env) args in  
      (* create an augmented memory state where input ids are bound to the *)
      (* argument values on the gpu *) 
      let env' = 
        List.fold_left2 
          (fun accEnv id dynval -> PMap.add id dynval accEnv)
          env 
          fundef.input_ids
          argDynVals
      in 
      let env'' = eval_block codeCache memState functions env' fundef.body in 
      List.map (fun id -> PMap.find id env'') fundef.output_ids  
      
  | ArrayIndex (arr, indices) -> 
        failwith "[eval] array indexing not implemented"
  | Arr elts -> failwith "[eval] array constructor not implemented"
  | Cast (t, valNode) -> failwith "[eval] cast not implemented" 
 
