(* pp: -parser o pa_macro.cmo *)
open Base 
open Type 
open Imp

(* assume all threadblocks are 1d row of size threadsPerBlock *) 
let gen_map payload threadsPerBlock closureTypes inTypes outTypes =
  IFDEF DEBUG THEN 
    Printf.printf "\n\nGenerating map template for %s%s -> %s\n"
      (if Array.length closureTypes = 0 then "" 
       else Type.type_array_to_str closureTypes ^ " => ")
      (Type.type_array_to_str inTypes)
      (Type.type_array_to_str outTypes)
    ; 
  ENDIF; 
  let nInputs = Array.length inTypes in 
  let nOutputs = Array.length outTypes in 
  assert (nInputs > 0 && nOutputs > 0);
  let fnState = new ImpCodegen.fn_state in 
  (* setup params for closure args, inputs, outputs *) 
  let closureArgs = Array.map fnState#fresh_input closureTypes in 
  let inputArgs = Array.map fnState#fresh_input inTypes in
  (* setup input and output variables for the payload fn *)
  let maxInput : Imp.exp_node = SymbolicShape.largest_val inputArgs in
  let maxShape = SymbolicShape.all_dims maxInput in 
  let maxDim : SymbolicShape.dim = SymbolicShape.outer_dim maxShape in       
  let closureArgShapes = 
    Array.map (fun c -> SymbolicShape.all_dims c) closureArgs
  in 
  let inputEltShapes = 
    Array.map 
      (fun inputVec -> 
        SymbolicShape.peel_shape (SymbolicShape.all_dims inputVec) 
      )
      inputArgs 
  in
  let inputEltTypes = Array.map Type.peel_vec inTypes in 
  let payloadInputShapes = Array.append closureArgShapes inputEltShapes in   
  let payloadOutputShapes = 
    Array.of_list $ SymbolicShape.get_call_output_shapes 
      payload 
      (Array.to_list payloadInputShapes)
  in
  let outEltTypes = Array.map Type.peel_vec outTypes in 
  (* outputs of payload function *) 
  let outVars = 
    Array.map2 
      (fun dims t -> fnState#fresh_var ~dims t) payloadOutputShapes outEltTypes 
  in
  let globalOutputShapes = 
    Array.map (fun shape -> maxDim::shape) payloadOutputShapes
  in
  let globalOutputArgs =
    Array.map2 
      (fun t dims -> fnState#fresh_output ~dims t) 
      outTypes 
      globalOutputShapes 
  in 
  let num = fnState#fresh_var Int32T in
  let mapIdx = fnState#fresh_var Int32T in
  let codeBuffer = fnState#main_code_buffer in 
  codeBuffer#emit [
    set mapIdx 
      (((blockIdx.x +$ (blockIdx.y *$  gridDim.x)) *$  (int threadsPerBlock))
       +$ threadIdx.x);
    set num (len globalOutputArgs.(0))
  ];

  let nestedBuffer = fnState#fresh_code_buffer in 
  (* same number of payload inputs as total inputs, but 
     change everything but the closure args to be elements 
     rather than full vectors 
   *)   
  let inputEltArgs = 
    Array.map2 
      (fun t dims -> fnState#fresh_var ~dims t)
      inputEltTypes 
      inputEltShapes
  in 
  for i = 0 to  nInputs - 1 do
    if Type.is_scalar inTypes.(i) then 
      (* assign elt to be the scalar input *)
      nestedBuffer#emit [set inputEltArgs.(i) inputArgs.(i)]
    else 
      nestedBuffer#emit [set inputEltArgs.(i) (idx inputArgs.(i) mapIdx)]
  done;
  let payloadInputs = Array.append closureArgs inputEltArgs in 
     
  (* this is where the payload gets inserted *) 
  nestedBuffer#emit [SPLICE];
  for i = 0 to nOutputs - 1 do  
     nestedBuffer#emit [set (idx globalOutputArgs.(i) mapIdx) outVars.(i)]
  done; 
  codeBuffer#splice_emit payload payloadInputs outVars
    [ifTrue (mapIdx <$ num) (nestedBuffer#to_block)];
  fnState#finalize
