open Base  


let map memState closureArgs dataArgs fundef = 
    let memoryCosts = 
      List.sum (List.map (MemoryState.host_transfer_time memState) dataArgs)
    in
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in 
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape 
      | None -> failwith "max shape not found"
    in 
    (* assume we process 100 elements per millisecond on the host, 
       but also assume nested computations are free
     *) 
    let runCost = (Shape.get maxShape 0) / 10 in 
    1 + memoryCosts + runCost  
    
let array_op memState op argVals = match op, argVals with 
  | _ -> 0 (* don't run anything else on the host *)  
