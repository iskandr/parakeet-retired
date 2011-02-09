

let run_map fundef inputTypes outputTypes hostVals =
  let hostVals = List.map (MemoryState.get_host memState dynVals) dynVals in 
  let shapes = List.map (fun hv -> hv.shape) hostVals in 
  let maxShape = match Shape.max_shape_list shapes with
     | None -> raise InvalidGpuArgs
     | Some maxShape -> maxShape
  in
  let outputVals =
    List.map
      (fun ty ->  
          let sz = sizeof eltT maxShape in 
          HostVal.mk_host_vec ty maxShape sz)
      outputTypes
  in
  let outputElts = Shape.nelts maxShape in
  
  