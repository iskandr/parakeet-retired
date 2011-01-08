  let map memState closureArgs dataArgs fundef =  
    let launchCost = 3  in
    (* assume we can transfer 100,000 elements per millsecond to GPU, 
           and that allocation costs 3ms no matter the size 
         *)
    
    let memoryCosts = 
      List.sum (List.map (MemoryState.gpu_transfer_time memState) dataArgs)
      +
      List.sum (List.map (MemoryState.gpu_transfer_time memState) closureArgs)
    in  
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in  
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape
      | None -> failwith "no common shape found"
    in  
    (* assume each processor can process 1000 elements per millisecond, and 
       we have 100 processors
    *)
    let runCost = Shape.nelts maxShape / 100  in 
    launchCost + memoryCosts + runCost 
  
  
  let array_op memState op argVals = match op, argVals with 
  | _ -> 0 