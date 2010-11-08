open Base 
open DynType 
open Imp
open ImpCodegen

let find_largest_input inVars inTypes =
  let maxType = ref inTypes.(0) in 
  (* find a variable of maximal type, 
     keep track of the corresponding variable 
  *)
  let maxVar = ref inVars.(0) in   
  for i = 1 to Array.length inTypes - 1 do
    if DynType.is_structure_subtype !maxType inTypes.(i) then 
      maxVar := inVars.(i)
  done; 
  !maxVar  
      

(* assume all threadblocks are 1d row of size threadsPerBlock *) 
let gen_map payload threadsPerBlock inTypes outTypes =
  assert (Array.length inTypes > 0);
  assert (Array.length outTypes > 0); 
  let codegen = new imp_codegen in 
  let inputArgs = Array.map codegen#fresh_input inTypes in
  let largestInput = find_largest_input inputArgs inTypes in
  let outputSizes = all_dims largestInput in  
  let outputArgs = 
    Array.map (fun t -> codegen#fresh_array_output t outputSizes) outTypes 
  in  
  let inEltTypes = Array.map DynType.peel_vec inTypes in 
  let outEltTypes = Array.map DynType.peel_vec outTypes in 
  let inVars = Array.map codegen#fresh_var inEltTypes in 
  
  let outVars = Array.map codegen#fresh_var outEltTypes in 
  let nInputs = Array.length inVars in
  let nOutputs = Array.length outVars in 
  let mapIdx = codegen#fresh_var UInt32T in
  codegen#emit [
    set mapIdx 
      (threadIdx.x +$ 
           (mul ~t:UInt32T blockIdx.x (int threadsPerBlock)) 
           +$ 
           (mul ~t:UInt32T blockIdx.y gridDim.x))
        
  ];  
  for i = 0 to nInputs - 1 do 
    if DynType.is_scalar inTypes.(i) then 
      (* assign elt to be the scalar input *)
      codegen#emit [set inVars.(i) inputArgs.(i)]
    else 
      codegen#emit [set inVars.(i) $ idx inputArgs.(i) mapIdx]
  done;
   
  codegen#splice_emit payload inVars outVars [SPLICE];
  for i = 0 to nOutputs - 1 do  
     codegen#emit [set (idx outputArgs.(i) mapIdx) outVars.(i)]
  done; 
  codegen#finalize
