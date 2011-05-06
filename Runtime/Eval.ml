(* pp: -parser o pa_macro.cmo *)

open Printf 
open Base

open SSA 
open InterpVal 
 

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
       
  let eval_value  (valNode : SSA.value_node) : InterpVal.t = 
    match valNode.value with 
    | Var id -> MemoryState.lookup P.memState id 
    | Num n -> InterpVal.Scalar n 
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
        (InterpVal.to_str rhsVal)
    ENDIF;  
    MemoryState.set_binding P.memState id rhsVal 
    
  let eval_phi_nodes cond phiNodes : unit =
    List.iter (eval_phi_node cond) phiNodes 
                        
  let rec eval_block block = Block.iter_forward eval_stmt block 
    
  and eval_stmt (stmtNode : SSA.stmt_node) : unit = 
    IFDEF DEBUG THEN 
      Printf.printf "\n===> EVAL: %s\n"
        (SSA.stmt_node_to_str stmtNode); 
    ENDIF; 
    match stmtNode.stmt with 
    | Set (ids, expNode) -> 
        let rhsVals = eval_exp expNode in 
        MemoryState.set_bindings P.memState ids rhsVals 
    | SetIdx (id, indices, rhs) -> assert false    
    | If (boolVal, tBlock, fBlock, phiNodes) ->
      let cond = InterpVal.to_bool (eval_value boolVal) in 
      eval_block (if cond then tBlock else fBlock);  
      eval_phi_nodes cond phiNodes
      
    | WhileLoop (testBlock, testVal, body, header) ->
      
      eval_phi_nodes true header;  
      eval_block testBlock;  
      let cond = ref (eval_value testVal) in
      let niters = ref 0 in   
      while InterpVal.to_bool !cond do        
        niters := !niters + 1; 
        eval_block body; 
        eval_phi_nodes false header; 
        eval_block testBlock; 
        cond := eval_value testVal; 
        IFDEF DEBUG THEN 
          Printf.printf "\n\n====> While Loop iteration %d\n%!" !niters; 
        ENDIF; 
      done
         
and eval_exp (expNode : SSA.exp_node) : InterpVal.t list = 
  match expNode.exp with 
  | Values valNodes -> List.map eval_value valNodes
  | Arr elts -> 
      [InterpVal.Array (Array.of_list (List.map eval_value elts))]
  | Cast (t, valNode) when DynType.is_scalar t -> 
      (match eval_value valNode with 
        | InterpVal.Scalar n -> [InterpVal.Scalar (PQNum.coerce_num n t)]
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
  
  | Map ({closure_fn=fnId; closure_args=closureArgs}, args) ->
      let fundef = get_fundef fnId in
      let closureArgVals : InterpVal.t list = List.map eval_value closureArgs in 
      let argVals : InterpVal.t list = List.map eval_value args in
      IFDEF DEBUG THEN
        Printf.printf "[Eval] args to interp map: %s %s\n"
          (String.concat ", " (List.map InterpVal.to_str closureArgVals))
          (String.concat ", " (List.map InterpVal.to_str argVals)); 
      ENDIF;  
      let bestLoc, bestTime = 
        CostModel.map_cost 
          P.fnTable 
          fundef 
          (describe_args closureArgVals)
          (describe_args argVals) 
      in 
      (match bestLoc with  
        | CostModel.GPU ->
            IFDEF DEBUG THEN Printf.printf "[Eval] running map on GPU\n" ENDIF;
            let gpuClosureVals = List.map get_gpu closureArgVals in 
            let gpuInputVals = List.map get_gpu argVals in   
            MemoryState.enter_data_scope P.memState; 
            let gpuResults = 
              GpuEval.map 
                ~payload:fundef
                ~closureArgs:gpuClosureVals  
                ~args:gpuInputVals
            in
            let interpResults = List.map add_gpu gpuResults in 
            MemoryState.exit_data_scope
              ~escaping_values:interpResults 
              P.memState
            ;
            interpResults 
        | CostModel.CPU -> 
            IFDEF DEBUG THEN Printf.printf "[Eval] running map on CPU\n" ENDIF;
            eval_map ~payload:fundef closureArgVals argVals   
      )
  | Reduce (initClosure, reduceClosure, initArgs, dataArgs)-> 
      let initFundef = get_fundef initClosure.closure_fn in
      (* the current reduce kernel works either for 1d data or 
         for maps over 2d data 
      *) 
       
      let initClosureArgs = 
        List.map eval_value initClosure.closure_args 
      in
      let reduceFundef = get_fundef reduceClosure.closure_fn in
      let reduceClosureArgs = 
        List.map eval_value reduceClosure.closure_args 
      in 
      let initArgVals = List.map eval_value initArgs in 
      let argVals  = List.map eval_value dataArgs in
      (match 
        CostModel.reduce_cost 
            ~fnTable:P.fnTable 
            ~init:initFundef
            ~initClosureArgs:(describe_args initClosureArgs)
            ~fn:reduceFundef
            ~closureArgs:(describe_args reduceClosureArgs)
            ~initArgs:(describe_args initArgVals)
            ~args:(describe_args argVals) 
      with 
        | CostModel.GPU, _ -> 
          MemoryState.enter_data_scope P.memState;           
          let gpuResults = 
            GpuEval.reduce
              ~init:initFundef
              ~initClosureArgs:(List.map get_gpu initClosureArgs)
              ~payload:reduceFundef 
              ~payloadClosureArgs:(List.map get_gpu reduceClosureArgs)
              ~initArgs:(List.map get_gpu initArgVals)
              ~args:(List.map get_gpu argVals) 
          in 
          let interpResults = List.map add_gpu gpuResults in 
          MemoryState.exit_data_scope ~escaping_values:interpResults P.memState;
          interpResults 

        | CostModel.CPU, _ ->   failwith "CPU reduction not implemented"
      )
  | Scan ({closure_fn=initFnId; closure_args=initClosureArgs}, 
          {closure_fn=fnId; closure_args=closureArgs}, initArgs, args) ->    
     failwith "scan not implemented"
  | _ -> 
      failwith $ Printf.sprintf 
        "[eval_exp] no implementation for: %s\n"
        (SSA.exp_to_str expNode)
             
  and eval_app fundef args =
    IFDEF DEBUG THEN 
      assert (List.length fundef.input_ids = List.length args); 
    ENDIF;
    MemoryState.enter_scope P.memState; 
    MemoryState.set_bindings P.memState fundef.input_ids args;
    eval_block fundef.body;
    let outputs = List.map (MemoryState.lookup P.memState) fundef.output_ids in 
    MemoryState.exit_scope ~escaping_values:outputs P.memState; 
    outputs
    
  and eval_scalar_op (op : Prim.scalar_op) (args : InterpVal.t list) =
    (* whether the scalar is a GpuVal, a HostVal or an interpreter scalar, 
       put them all into hostvals 
    *)
    let nums = List.map (MemoryState.get_scalar P.memState) args in    
    match op, nums  with 
    | Prim.Eq, [x;y] -> InterpVal.of_bool (x = y) 
    | Prim.Neq, [x;y] -> InterpVal.of_bool (x <> y) 
    | Prim.Lt,  [x;y] ->
        InterpVal.of_bool (PQNum.to_float x <= PQNum.to_float y)  
    | Prim.Lte, [x;y] -> 
        InterpVal.of_bool (PQNum.to_float x <= PQNum.to_float y)  
    | Prim.Gt, [x;y] -> 
        InterpVal.of_bool (PQNum.to_float x > PQNum.to_float y)
    | Prim.Gte,  [x;y] -> 
        InterpVal.of_bool (PQNum.to_float x >= PQNum.to_float y)
    | Prim.Div, [x;y] -> 
        InterpVal.of_float (PQNum.to_float x /. PQNum.to_float y)
    (* other math operations should return the same type as their inputs, 
       so use the generic math eval function
     *) 
    | op, _  -> 
        InterpVal.Scalar (MathEval.eval_pqnum_op op nums)
  and eval_array_op 
        (op : Prim.array_op) 
        (argVals : InterpVal.t list)
        (outTypes : DynType.t list) : InterpVal.t list =
    (*
       push a scope to clean up any 
       non-escaping temporaries generated by array operators
    *) 
    MemoryState.enter_data_scope P.memState; 
    let results = match op, argVals with
    | Prim.Index, [array; idx] ->
      (* always run on GPU *)
      let gpuArr = get_gpu array in 
      if DynType.is_scalar (typeof idx) then (
        if DynType.nest_depth (typeof array) = 1 then  
          let gpuVec = GpuVal.get_gpu_vec gpuArr in 
          let n = GpuVal.get_elt gpuVec (InterpVal.to_int idx) in 
          [InterpVal.Scalar n]
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
        "Where's my scalar? I ordered a scalar: %s (host: %s)"
        (String.concat ", " (List.map InterpVal.to_str others))
        (String.concat ", " 
          (List.map HostVal.to_str 
            (List.map (MemoryState.get_host P.memState) others))) 

     | op, args  ->  
        failwith $ Printf.sprintf "CPU operator not implemented: %s for args %s"
          (Prim.array_op_to_str op)
          (String.concat ", " (List.map InterpVal.to_str args)) 
     in 
     MemoryState.exit_data_scope ~escaping_values:results  P.memState; 
     results 

  and eval_map ~payload closureArgs argVals =
    IFDEF DEBUG THEN Printf.printf "Running MAP on host!\n"; ENDIF; 
    let dataShapes = List.map (MemoryState.get_shape P.memState) argVals in
    let maxShape = match Shape.max_shape_list dataShapes with 
    | Some maxShape -> maxShape 
    | None -> 
      failwith "Incompatible array shapes encountered while evaluating Map"
    in 
    (* if trying to map over scalars, just evaluate this function directly *)  
    if Shape.rank maxShape = 0 then eval_app payload argVals    
    else 
    let n = Shape.get maxShape 0 in
    let outputIds = Array.of_list (payload.output_ids) in
    let nOutputs = Array.length outputIds in  
    let allResults = Array.init nOutputs  (fun _ -> DynArray.create ()) in
    let get_slice idx v =
      IFDEF DEBUG THEN 
        Printf.printf "[Eval] Getting slice %d of %s\n%!"
          idx
          (InterpVal.to_str v) 
          ;
      ENDIF;   
      let t = MemoryState.get_type P.memState v in 
      if DynType.is_vec t then MemoryState.slice P.memState v idx
      else v    
    in 
    for elt = 0 to n - 1 do
      let slices = List.map (get_slice elt) argVals in
      let inputs = (closureArgs @ slices) in
      let currResults = Array.of_list (eval_app payload inputs) in 
      for i = 0 to nOutputs - 1 do 
        DynArray.add allResults.(i) currResults.(i)
      done;    
    done;   
    let mk_array dynArray = InterpVal.Array (DynArray.to_array dynArray) in  
    Array.to_list $ Array.map mk_array  allResults  
end

let eval globalFns fundef hostVals =
  let memState = MemoryState.create ()  in  
  let vals = List.map (fun h -> MemoryState.add_host memState h) hostVals in 
  (* parameterize the evaluator by mutable global function table and memory *)
  let module E = 
    Mk(struct let fnTable = globalFns let memState = memState end) 
  in  
  let outputVals = E.eval_app fundef vals in
  let results = List.map (MemoryState.get_host memState) outputVals in  
  MemoryState.flush_gpu memState; 
  results 
