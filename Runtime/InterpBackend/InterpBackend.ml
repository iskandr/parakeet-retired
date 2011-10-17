(* pp: -parser o pa_macro.cmo *)


module HostMemspace = struct end 

module Memspace = HostMemspace

(* QUESTION: How call back into Interp when it depends on a particular *)
(* memState--- should I make that global? *) 

let map fn closureArgs args axes = 
  assert (axes = [0]); (* to do: make this work for arbitrary axes *) 
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
      if Type.is_vec t then MemoryState.slice P.memState v idx
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
let reduce fn closure_args args axes = []
let scan fn closure_args args axes = []

    
            