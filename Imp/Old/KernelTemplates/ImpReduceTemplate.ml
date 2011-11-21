(* pp: -parser o pa_macro.cmo *)

open Base
open Type
open Imp
open ImpCodegen

(* ty = type of vector elements, which confusingly may differ from the 
   input type of the payload for 2d reductions 
*) 
let gen_reduce_2d_capable nestedType payload threadsPerBlock =
  (* For now only works on 1D and 2D inputs *)
  let fnState = new ImpCodegen.fn_state  in
  IFDEF DEBUG THEN 
    assert (Array.length payload.input_types = 2);
    if Array.length payload.output_types <> 1 then 
      failwith $ Printf.sprintf 
        "[ImpReduceTemplate] Function has return values %s, expected only 1"
        (Type.type_array_to_str payload.output_types);
  ENDIF; 
  (* assume result of reduction is same as elements of vector *)
  let inputVecType = Type.VecT nestedType in  
  let input = fnState#fresh_input inputVecType in
  let scalarType = Type.elt_type nestedType in
  match SymbolicShape.all_dims input with
  | [] -> failwith "[ImpReduceTemplate] Unexpected scalar input" 
  | outerDim::nestedShape -> 
  let reducedDim = safe_div_ outerDim (int $ threadsPerBlock * 2) in 
  let outputShape = reducedDim::nestedShape in 
  (* the GPU kernel doesn't fully reduce its input--- 
     that only happens after a logarithmic number of invocations, 
    so the type of the output variable is the same as the input 
  *) 
  let output = fnState#fresh_output ~dims:outputShape inputVecType in
  IFDEF DEBUG THEN
    Printf.printf 
      "Creating reduce kernel with output shape {%s} w/ payload (%s) -> (%s)\n"
      (SymbolicShape.to_str outputShape) 
      (Type.type_array_to_str payload.input_types)
      (Type.type_array_to_str payload.output_types);
  ENDIF;  
  let cache = 
    fnState#shared_vec_var (Type.VecT scalarType) [threadsPerBlock] 
  in
  let num_vecs = fnState#fresh_var Int32T in
  let codeBuffer = fnState#main_code_buffer in 
  codeBuffer#emit [set num_vecs (len input)];
  
  let vec_len = fnState#fresh_var Int32T in
  let ndims = Type.rank nestedType + 1 in
  if ndims = 1 then
    codeBuffer#emit [set vec_len (int 1)]
  else if ndims = 2 then
    codeBuffer#emit [set vec_len (dim 1 input)]
  else
    failwith "Reduce only supported on 1D or 2D inputs for now";
  
  let spin1 = fnState#fresh_var scalarType in
  let spin2 = fnState#fresh_var scalarType in
  let spout = fnState#fresh_var scalarType in
  
  let bx = fnState#fresh_var Int32T in
  let by = fnState#fresh_var Int32T in
  codeBuffer#emit [
    comment "bx = blockDim.x"; 
    set bx blockDim.x;
    comment "by = blockDim.y";
    set by blockDim.y
  ];
  
  let id_x = fnState#fresh_var Int32T in
  let id_y = fnState#fresh_var Int32T in
  codeBuffer#emit [
    comment "idx_x = blockIdx.x * bx + threadIdx.x";
    set id_x ((blockIdx.x *$ bx) +$ threadIdx.x);
    comment "id_y = blockIdx.y * by + threadIdx.y";
    set id_y ((blockIdx.y *$ by) +$ threadIdx.y)
  ];
  
  let firstvec = fnState#fresh_var Int32T in
  let cacheid = fnState#fresh_var Int32T in
  let cur_y = fnState#fresh_var Int32T in
  
  let fetch1 =
    if ndims = 1 then
      setidx cache [cacheid] (idx input firstvec)
    else
      setidx cache [cacheid] (idx (idx input firstvec) id_x)
  in
  let fetch2 =
    if ndims = 1 then
      set spin2 (idx input (firstvec +$ (int 1)))
    else
      set spin2 (idx (idx input (firstvec +$ (int 1))) id_x)
  in
  let outidxs =
    if ndims = 1 then
      [blockIdx.y]
    else
      [blockIdx.y;id_x]
  in
  
  let template = [
    comment "start of template";
    ifTrue (id_x <$ vec_len) [
	    set firstvec ((int 2) *$ id_y);
	    set cacheid ((bx *$ threadIdx.y) +$ threadIdx.x);
      
	    ifTrue (firstvec <$ num_vecs) [fetch1];
	    ifTrue ((firstvec +$ (int 1)) <$ num_vecs) [
	      set spin1 (idx cache cacheid);
        fetch2;
	      SPLICE;
	      setidx cache [cacheid] spout
	    ];
	    syncthreads;
	  
	    (* TODO: Redo this as a bit-shift *)
	    set cur_y (by /$ (int 2));
	    while_ (cur_y >$ (int 0)) [
	      ifTrue ((threadIdx.y <$ cur_y) &&$
                (((id_y +$ cur_y) *$ (int 2)) <$ num_vecs)) [
	        set spin1 (idx cache cacheid);
	        set spin2 (idx cache (cacheid +$ (cur_y *$ bx)));
	        SPLICE;
	        setidx cache [cacheid] spout
	      ];
	      syncthreads;
	      set cur_y (cur_y /$ (int 2))
	    ];
	    ifTrue ((threadIdx.y =$ (int 0)) &&$ (threadIdx.x <$ bx)) [
	      setidx output outidxs (idx cache threadIdx.x)
	    ]
    ]
  ] in
  codeBuffer#splice_emit payload [|spin1;spin2|] [|spout|] template;
  fnState#finalize

