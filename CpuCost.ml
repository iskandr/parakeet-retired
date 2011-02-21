open Base  


let map memState closureArgs dataArgs fundef = 
    let memoryCosts = 
      List.sum (List.map (MemoryState.host_transfer_time memState) dataArgs)
    in
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in
    IFDEF DEBUG THEN 
      Printf.printf "shapes: %s\n" 
        (String.concat "; " (List.map Shape.to_str shapes))
    ENDIF; 
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape 
      | None -> failwith "max shape not found"
    in
    IFDEF DEBUG THEN 
      Printf.printf "max shape: %s\n" (Shape.to_str maxShape);  
    ENDIF;  
    (* assume we process 100 elements per millisecond on the host, 
       but also assume nested computations are free
     *) 
    assert (Shape.rank maxShape > 0); 
    let runCost = (Shape.get maxShape 0) / 10 in 
    1 + memoryCosts + runCost  
    
let array_op memState op argVals = match op, argVals with 
  | _ -> 0 (* don't run anything else on the host *)  
