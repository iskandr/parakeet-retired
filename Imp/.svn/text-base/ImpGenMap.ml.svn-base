open Base 
open DynType 
open Imp
open ImpCodegen

(* assume all threadblocks are 1d row of size threadsPerBlock *) 
let gen_map payload threadsPerBlock inTypes outTypes = 
  print "gen_map_payload";
  let codegen = new imp_codegen in 
  let inputArgs = Array.map codegen#fresh_input inTypes in 
  let outputArgs = Array.map codegen#fresh_output outTypes in  
  let inEltTypes = Array.map DynType.peel_vec inTypes in 
  let outEltTypes = Array.map DynType.peel_vec outTypes in 
  let inVars = Array.map codegen#fresh_var inEltTypes in 
  let outVars = Array.map codegen#fresh_var outEltTypes in 
  let nInputs = Array.length inVars in
  let nOutputs = Array.length outVars in 
  let mapIdx = codegen#fresh_var Int32T in
  codegen#emit [
    set mapIdx 
      (add Int32T threadIdx.x 
        (add  Int32T           
           (mul Int32T blockIdx.x (int threadsPerBlock))
           (mul Int32T blockIdx.y gridDim.x)
        ))
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
   
  
