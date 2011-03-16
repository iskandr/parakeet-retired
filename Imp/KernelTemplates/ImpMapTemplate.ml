(* pp: -parser o pa_macro.cmo *)
open Base 
open DynType 
open Imp
open ImpCodegen


(* assume all threadblocks are 1d row of size threadsPerBlock *) 
let gen_map payload threadsPerBlock closureTypes inTypes outTypes =
  IFDEF DEBUG THEN 
    Printf.printf "\n\nGenerating map template for %s%s -> %s\n"
      (if Array.length closureTypes = 0 then "" 
       else DynType.type_array_to_str closureTypes ^ " => ")
      (DynType.type_array_to_str inTypes)
      (DynType.type_array_to_str outTypes)
    ; 
  ENDIF; 
  let nClosureArgs = Array.length closureTypes in
  let nInputs = Array.length inTypes in 
  let nOutputs = Array.length outTypes in 
  assert (nInputs > 0 && nOutputs > 0);
  let codegen = new imp_codegen in 
  (* setup params for closure args, inputs, outputs *) 
  let closureArgs = Array.map codegen#fresh_input closureTypes in 
  let inputArgs = Array.map codegen#fresh_input inTypes in
  let outputSizes = 
    SymbolicShape.all_dims (SymbolicShape.largest_val inputArgs) 
  in
  let outputArgs = 
    Array.map (fun t -> codegen#fresh_output ~dims:outputSizes t) outTypes 
  in 
  let num = codegen#fresh_var Int32T in
  let mapIdx = codegen#fresh_var Int32T in
  codegen#emit [
    set mapIdx 
      (((blockIdx.x +$ (blockIdx.y *$  gridDim.x)) *$  (int threadsPerBlock))
       +$ threadIdx.x);
    set num (len outputArgs.(0))
  ];
  (* setup input and output variables for the payload fn *)
  let inputEltTypes = Array.map DynType.peel_vec inTypes in
  let inputEltShapes = 
    Array.map 
      (fun varNode -> SymbolicShape.peel_shape (SymbolicShape.all_dims varNode))
    inputArgs 
  in   
  let mk_payload_input kernelInput = 
    let t = DynType.peel_vec kernelInput.exp_type in 
    let dims = SymbolicShape.peel_shape (SymbolicShape.all_dims kernelInput) in
    IFDEF DEBUG THEN 
      Printf.printf "Registering payload input of type %s and shape [%s]\n"
      (DynType.to_str t)
      (SymbolicShape.to_str dims)
    ENDIF; 
    codegen#fresh_var t ~dims ~storage:Imp.Slice   
  in 
  let payloadInputVars = 
    Array.append closureArgs (Array.map mk_payload_input inputArgs)
  in 
  let buffer = DynArray.create () in
  (* put each input in payloadInputVars *) 
  for i = nClosureArgs to nClosureArgs + nInputs - 1 do 
    let inputIdx = i - nClosureArgs in 
    if DynType.is_scalar inTypes.(inputIdx) then 
      (* assign elt to be the scalar input *)
      DynArray.add buffer 
        (set payloadInputVars.(i) inputArgs.(inputIdx))
    else 
      DynArray.add buffer 
        (set payloadInputVars.(i) (idx inputArgs.(inputIdx) mapIdx))
  done;
   
  (* this is where the payload gets inserted *) 
  DynArray.add buffer (SPLICE);
  
  let outEltTypes = Array.map DynType.peel_vec outTypes in 
  let outVars = Array.map codegen#fresh_var outEltTypes in
  for i = 0 to nOutputs - 1 do  
     DynArray.add buffer (set (idx outputArgs.(i) mapIdx) outVars.(i))
  done; 
  
  codegen#splice_emit payload payloadInputVars outVars
    [ifTrue (mapIdx <$ num) (DynArray.to_list buffer)];
  codegen#finalize
  
