open Base  

type compute_location = GPU | CPU 

module type COST_MODEL_PARAMS = sig 
  val fnTable : FnTable.t 
  val memState : MemoryState.t 
end 

module Mk(P : COST_MODEL_PARAMS) = struct
  let rec map_cost ~fn ~closureArgs ~dataArgs  = 
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape 
      | None -> failwith "max shape not found"
    in
    let gpuCost = 
    let nestedCost =  
    (* assume we process 100 elements per millisecond on the host, 
       but also assume nested computations are free
     *) 
    assert (Shape.rank maxShape > 0); 
    let runCost = (Shape.get maxShape 0) / 10 in 
    CPU, 1 +  runCost  
  and call_cost ~memState     
  
  let array_op op argVals = match op, argVals with 
    | _ -> CPU, 0 (* don't run anything else on the host *)  
end