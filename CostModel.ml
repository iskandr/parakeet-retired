open Base  

type compute_location = GPU | CPU 
type cost = int
 
module type COST_MODEL_PARAMS = sig 
  val fnTable : FnTable.t 
  val memState : MemoryState.t 
end 

module Mk(P : COST_MODEL_PARAMS) = struct 
  module G = GpuCost.Mk(struct let fnTable = fnTable end) 
  
  type env = { 
    types : DynType.t ID.Map.t; 
    shapes : Shape.t ID.Map.t; 
    onGpu : ID.Set.t; 
  } 
  
  type exp_info = compute_location * cost  
  type value_info = Shape.t * DynType.t * bool 
    
  let dir = SSA_Analysis.Forward 
  let iterative = false 
  
  let init fundef = {  
    
  } 
    val value : env -> value_node -> value_info
    
    val exp : env -> exp_node -> (env, value_info) helpers -> exp_info
    val phi : env -> env -> env -> SSA.phi_node -> env option 
    val stmt : env -> stmt_node -> (env, value_info) helpers -> env option 

   
  let max_arg_shape args = 
    let argShapes = List.map (fun (s,_,_) -> s) args in 
    match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape 
      | None -> failwith "max shape not found" 
  
  (* split args into max dim, and peeled inner args *) 
  let split_args args =
    let maxShape = max_arg_shape args in
    assert (Shape.rank maxShape > 0);
    let peeler (shape,ty,onGpu) =
      if Shape.eq shape maxShape then 
        Shape.peel_shape shape, DynType.peel_vec ty, onGpu  
      else shape, ty, onGpu 
    in  
    Shape.get maxShape 0, List.map peeler args 
  
  let peel_args args = snd (split_args args)   
  
   
  let rec block_cost block (argEnv : (Shape.t * DynType.t * bool) ID.Map.t) =
    (* start with zero cost...and add up all the stmtCosts *) 
    Block.fold_forward 
      (fun accCost stmtNode -> accCost + stmt_cost argEnv stmtNode) 
      0
      block
  and stmt_cost argEnv stmtNode = match stmtNode.stmt with 
    | SSA.Set(_, rhs) -> let _, cost  = exp_cost argEnv rhs in cost
    | _ -> failwith "not yet supported"   
  and exp_cost shapeEnv locEnv  
  and  map_cost fn closureArgs args =  
    let gpuCost = G.map fn closureArgShapes args in
    let maxDim, nestedArgs = split_args args in   
    let nestedCost = call_cost f (closureArgs @ nestedArgs) in   
    let cpuCost = 1 + maxDim * nestedCost in 
    if cpuCost < gpuCost then CPU, cpuCost 
    else GPU, gpuCost  
  
  and call_cost fn args =   
  
  let array_op op argVals = match op, argVals with 
    | _ -> CPU, 0 (* don't run anything else on the host *)  
end