open Base  

type compute_location = GPU | CPU 

module type COST_MODEL_PARAMS = sig 
  val fnTable : FnTable.t 
  val memState : MemoryState.t 
end 

module Mk(P : COST_MODEL_PARAMS) = struct
  
  module G = GpuCost.Mk(struct let fnTable = fnTable end) 
  
   
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
      let shape' = 
        if Shape.eq shape maxShape then Shape.peel_shape shape else shape
      in 
      shape', ty, onGpu
    in  
    Shape.get maxShape 0, List.map peeler args 
  
  let peel_args args = snd (split_args args)   
  
  let rec map_cost fn closureArgs args =  
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