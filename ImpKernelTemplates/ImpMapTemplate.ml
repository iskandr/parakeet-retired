open Base 
open DynType 
open Imp
open ImpCodegen


(* assume all threadblocks are 1d row of size threadsPerBlock *) 
let gen_map payload threadsPerBlock inTypes outTypes =
  assert (Array.length inTypes > 0);
  assert (Array.length outTypes > 0); 
  let codegen = new imp_codegen in 
  let inputArgs = Array.map codegen#fresh_input inTypes in
  let outputSizes = all_dims (largest_val inputArgs) in
  let outputArgs = 
    Array.map (fun t -> codegen#fresh_array_output t outputSizes) outTypes 
  in  
  let inEltTypes = Array.map DynType.peel_vec inTypes in 
  let outEltTypes = Array.map DynType.peel_vec outTypes in 
  let inVars = Array.map codegen#fresh_var inEltTypes in 
  let outVars = Array.map codegen#fresh_var outEltTypes in 
  let nInputs = Array.length inVars in
  let nOutputs = Array.length outVars in 
  let num = codegen#fresh_var Int32T in
  let mapIdx = codegen#fresh_var UInt32T in
  codegen#emit [
    set mapIdx 
      (((blockIdx.x +$ (blockIdx.y *$  gridDim.x)) *$  (int threadsPerBlock))
       +$ threadIdx.x);
    set num (len outputArgs.(0))
  ];
  let buffer = DynArray.create () in
  for i = 0 to nInputs - 1 do 
    if DynType.is_scalar inTypes.(i) then 
      (* assign elt to be the scalar input *)
      DynArray.add buffer (set inVars.(i) inputArgs.(i))
    else 
      DynArray.add buffer (set inVars.(i) $ idx inputArgs.(i) mapIdx)
  done;
  
  DynArray.add buffer (SPLICE);
  
  for i = 0 to nOutputs - 1 do  
     DynArray.add buffer (set (idx outputArgs.(i) mapIdx) outVars.(i))
  done; 
  
  codegen#splice_emit payload inVars outVars
    [ifTrue (mapIdx <$ num) (DynArray.to_list buffer)];
  codegen#finalize
