open Base  
open SSA 
type compute_location = GPU | CPU 
type cost = float 
 
  let describe_value 
        (shapeEnv : Shape.t ID.Map.t)
        (gpuSet : ID.Set.t) 
        (valNode : SSA.value_node) : DynType.t * Shape.t * bool =
    let t = valNode.value_type in 
    match valNode.value with 
      | Var id ->
        let shape = ID.Map.find id shapeEnv in 
        let onGpu = ID.Set.mem id gpuSet in 
        t, shape, onGpu 
      | _ ->  t, Shape.scalar_shape, true

  let rec describe_values shapeEnv gpuSet = function  
    | [] -> []
    | v::vs -> 
      let rest = describe_values shapeEnv gpuSet vs in 
      (describe_value shapeEnv gpuSet v) :: rest 

  let get_shape (t,s,onGpu) = s 
  let get_shapes argInfo = List.map get_shape argInfo 
  
  (* split args into max dim, and peeled inner args *) 
  let split_args args =
    let maxShape = Option.get (Shape.max_shape_list (get_shapes args)) in
    assert (Shape.rank maxShape > 0);
    let peeler (ty,shape,gpuSet) =
      if Shape.eq shape maxShape then 
        DynType.peel_vec ty, Shape.peel_shape shape, gpuSet  
      else  ty, shape, gpuSet 
    in  
    Shape.get maxShape 0, List.map peeler args 
  
  let peel_args args = snd (split_args args)   
  
  
  let val_node_on_gpu gpuSet valNode = match valNode.value with 
    | Var id -> ID.Set.mem id gpuSet 
    | _ -> true 
 
  (* since shape information has already been fully inferred in the *)
  (* ShapeInference module, phi nodes only have to merge information *)
  (* flow about whether a piece of data is on the GPU.  *)
  let phi_node gpuSet phiNode = 
    let leftOnGpu = val_node_on_gpu gpuSet phiNode.phi_left in 
    let rightOnGpu = val_node_on_gpu gpuSet phiNode.phi_right in 
    if leftOnGpu && rightOnGpu then ID.Set.add phiNode.phi_id gpuSet else gpuSet  
  
  let rec phi_nodes gpuSet = function 
    | [] -> gpuSet
    | node::nodes -> 
      let gpuSet' = phi_node gpuSet node in 
      phi_nodes gpuSet' nodes 

  let rec block_cost 
            (fnTable : FnTable.t) 
            (shapeEnv : Shape.t ID.Map.t) 
            (gpuSet : ID.Set.t) 
            block : cost * ID.Set.t  = 
    (* start with zero cost...and add up all the stmtCosts *) 
    Block.fold_forward 
      (fun (accCost, gpuSet) stmtNode -> 
        let currCost, gpuSet' =  stmt_cost fnTable shapeEnv gpuSet stmtNode in 
        accCost +. currCost, gpuSet' 
      ) 
      (0., gpuSet)
      block
  and stmt_cost 
        (fnTable : FnTable.t) 
        (shapeEnv : Shape.t ID.Map.t)
        (gpuSet : ID.Set.t) 
        (stmtNode : SSA.stmt_node) : cost * ID.Set.t = match stmtNode.stmt with 
    | Set(_, rhs) -> 
        let _, cost  = exp_cost fnTable shapeEnv gpuSet rhs in cost, gpuSet
    | SetIdx(_, indices, _) -> float_of_int (List.length indices), gpuSet   
    | WhileLoop (condBlock, _, body, header, exit) -> 
        let gpuSet' = phi_nodes gpuSet header in 
        let condCost, condGpuSet = 
          block_cost fnTable shapeEnv gpuSet' condBlock 
        in 
        let bodyCost, bodyGpuSet = 
          block_cost fnTable shapeEnv condGpuSet body 
        in
        let exitGpuSet = phi_nodes bodyGpuSet exit in 
        let totalCost = condCost +. bodyCost in 
        totalCost, exitGpuSet  
    | If(_, tBlock, fBlock, merge) -> 
        let tCost, tGpuSet = block_cost fnTable shapeEnv gpuSet tBlock in 
        let fCost, fGpuSet = block_cost fnTable shapeEnv tGpuSet fBlock in 
        let finalGpuSet = phi_nodes fGpuSet merge in 
        let totalCost = tCost +. fCost in 
        totalCost, finalGpuSet 
  and exp_cost 
        (fnTable : FnTable.t)
        (shapeEnv : Shape.t ID.Map.t)
        (gpuSet : ID.Set.t)
        (expNode : SSA.exp_node) : compute_location * cost = 
          match expNode.exp with 
            | Map(closure, args) ->
              let fundef = FnTable.find closure.closure_fn fnTable in
              let closureArgInfo = 
                describe_values shapeEnv gpuSet closure.closure_args 
              in
              let argInfo = describe_values shapeEnv gpuSet args in     
              map_cost fnTable fundef closureArgInfo argInfo 
            | _ -> CPU, 1.

  and  map_cost fnTable fn closureArgs args : compute_location * cost =  
    let gpuCost : cost = 
      GpuCost.map fnTable fn (get_shapes closureArgs) (get_shapes args) 
    in
    let maxDim, nestedArgs = split_args args in   
    let nestedCost = call_cost fnTable fn (closureArgs @ nestedArgs) in   
    let cpuCost = 1. +. (float_of_int maxDim) *. nestedCost in
    IFDEF DEBUG THEN 
      Printf.printf "Compute MAP cost: GPU - %f, HOST: %f\n" gpuCost cpuCost; 
    ENDIF; 
    if cpuCost < gpuCost then CPU, cpuCost 
    else GPU, gpuCost  
  
  and reduce_cost 
        ~fnTable 
        ~init 
        ~initClosureArgs 
        ~fn 
        ~closureArgs 
        ~initArgs 
        ~args =   
    let gpuCost : cost = 
      GpuCost.reduce 
        ~fnTable 
        ~init
        ~initClosureArgs:(get_shapes initClosureArgs)
        ~fn 
        ~closureArgs:(get_shapes closureArgs) 
        ~initArgs:(get_shapes initArgs) 
        ~args:(get_shapes args) 
    in
    let maxDim, nestedArgs = split_args args in   
    let nestedCost = call_cost fnTable fn (closureArgs @ nestedArgs) in   
    let cpuCost = 1. +. (float_of_int maxDim) *. nestedCost in
    IFDEF DEBUG THEN 
      Printf.printf "Compute REDUCE cost: GPU - %f, HOST: %f\n" gpuCost cpuCost; 
    ENDIF; 
    if cpuCost < gpuCost then CPU, cpuCost 
    else GPU, gpuCost  
  
  
  and call_cost fnTable fn argInfo  : cost =
    let mkSet gpuSet id (_,_,onGpu) = 
      if onGpu then ID.Set.add id gpuSet else gpuSet 
    in 
    let gpuSet = List.fold_left2 mkSet ID.Set.empty fn.input_ids argInfo in  
    let shapes = get_shapes argInfo in 
    let shapeEnv = ShapeEval.eval_ssa_shape_env fnTable fn shapes in 
    let cost, _ = block_cost fnTable shapeEnv gpuSet fn.body in 
    cost   
  
  let array_op op argVals = match op, argVals with 
    | _ -> CPU, 0. (* don't run anything else on the host *)  
