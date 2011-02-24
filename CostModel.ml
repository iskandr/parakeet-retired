open Base  

type runtime_location = GPU | CPU 

let map ~memState ~fnTable ~fn ~closureArgs ~dataArgs  = 
    let shapes = List.map (MemoryState.get_shape memState) dataArgs in
    let maxShape = match Shape.max_shape_list shapes with 
      | Some maxShape -> maxShape 
      | None -> failwith "max shape not found"
    in
    (* assume we process 100 elements per millisecond on the host, 
       but also assume nested computations are free
     *) 
    assert (Shape.rank maxShape > 0); 
    let runCost = (Shape.get maxShape 0) / 10 in 
    CPU, 1 +  runCost  
    
let array_op memState op argVals = match op, argVals with 
  | _ -> CPU, 0 (* don't run anything else on the host *)  
