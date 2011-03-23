(* pp: -parser o pa_macro.cmo *)

open Base
open DynType
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
        (DynType.type_array_to_str payload.output_types);
  ENDIF; 
  (* assume result of reduction is same as elements of vector *)
  let inputVecType = DynType.VecT nestedType in  
  let input = fnState#fresh_input inputVecType in
  let scalarType = DynType.elt_type nestedType in
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
      (DynType.type_array_to_str payload.input_types)
      (DynType.type_array_to_str payload.output_types);
  ENDIF;  
  let cache = 
    fnState#shared_vec_var (DynType.VecT scalarType) [threadsPerBlock] 
  in
  let num_vecs = fnState#fresh_var Int32T in
  let codeBuffer = fnState#main_code_buffer in 
  codeBuffer#emit [set num_vecs (len input)];
  
  let vec_len = fnState#fresh_var Int32T in
  let ndims = DynType.nest_depth nestedType + 1 in
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

(* reduces each block's subvector to a single output element *) 
(*
let gen_reduce_old payload threadsPerBlock =
  assert (Array.length payload.output_types = 2);
  (* assume result of reduction is same as elements of vector *) 
  let ty = payload.output_types.(1) in
  let fnState = new imp_fnState in
  let input = fnState#fresh_input (VecT ty) in
  let eltype = DynType.elt_type ty in
  let head::tail = SymbolicShape.all_dims input in
  let outputDims =  ((safe_div_ head (int $ threadsPerBlock * 2))::tail) in 
  let output = fnState#fresh_output ~dims:outputDims (VecT ty) in

  let cache = fnState#shared_vec_var eltype [threadsPerBlock] in

  let tid = fnState#fresh_var UInt32T in
  let linearBlockIdx = fnState#fresh_var UInt32T in
  let startBlock = fnState#fresh_var UInt32T in
  let i = fnState#fresh_var UInt32T in
  codeBuffer#emit [
    set tid threadIdx.x;
    set linearBlockIdx (blockIdx.x +$ (blockIdx.y *$ gridDim.x));
    set startBlock ((int $ threadsPerBlock * 2) *$ linearBlockIdx);
    set i (threadIdx.x +$ startBlock)
  ];
  
  let temp = fnState#fresh_var eltype in
  let tmp1 = fnState#fresh_var eltype in
  let tmp2 = fnState#fresh_var eltype in
  let template = [
    ifTrue (i <$ len input) [
      if_ ((i +$ (int threadsPerBlock)) <$ len input) [
        set tmp1 (idx input i);
        set tmp2 (idx input (i +$ (int threadsPerBlock)));
        SPLICE;
        setidx cache [tid] temp
      ] [
        setidx cache [tid] (idx input i)
      ]
    ];
    syncthreads
  ]
  in
  codeBuffer#splice_emit payload [|tmp1;tmp2|] [|temp|] template;
  
  let lenBlock = fnState#fresh_var Int32T in
  let tmpInt = fnState#fresh_var Int32T in
  codeBuffer#emit [
    set lenBlock (int $ threadsPerBlock * 2);
    set tmpInt ((len input) -$ startBlock);
    ifTrue (tmpInt <$ lenBlock) [
      set lenBlock tmpInt 
    ]
  ];
  
  let j = ref (threadsPerBlock / 2) in
  while !j >= 1 do
    let template = [
      ifTrue (tid <$ (int !j) &&$ (tid +$ (int !j) <$ lenBlock))
         [SPLICE; set (idx cache tid) temp];
      syncthreads
    ] in
    let loopPInputs =
      [|idx cache $ tid +$ (int !j); idx cache tid|] in
    codeBuffer#splice_emit payload loopPInputs [|temp|] template;
    j := !j / 2
  done;
  
  codeBuffer#emit [ifTrue (tid =$ (int 0))
                 [set (idx output linearBlockIdx) (idx cache $ int 0)]
               ];
  fnState#finalize

(* reduces each block's subvector to a single output element *) 
let gen_reduce_opt payload threadsPerBlock ty =
  let fnState = new imp_fnState in
  let input = fnState#fresh_input (VecT ty) in
  let output = fnState#fresh_output (VecT ty) in
  let cache = fnState#shared_vec_var ty [threadsPerBlock] in 
  let tid = fnState#fresh_var Int32T in 
  codeBuffer#emit [
    set tid threadIdx.x
  ];
  let i = fnState#fresh_var Int32T in
  let linearBlockIdx = fnState#fresh_var Int32T in
  codeBuffer#emit [
    set linearBlockIdx (add blockIdx.x
                        (mul blockIdx.y gridDim.x));
    set i (add threadIdx.x
            (mul linearBlockIdx (int $ threadsPerBlock * 2)));
    set (idx cache tid) (idx input i)
  ];
  let temp = fnState#fresh_var ty in
  let inputs =
    [|idx input $ add i (int threadsPerBlock); idx cache tid |] in
  let template = [SPLICE; setidx cache [tid] temp] in
  let trueBranch = codeBuffer#splice payload inputs [|temp|] template in
  let condition =
    lt (add i (int threadsPerBlock)) (len input) in
  
  codeBuffer#emit [ifTrue condition trueBranch; syncthreads];
  let j = ref (threadsPerBlock / 2) in 
  while !j >= 64 do
    let template = [
      ifTrue (lt tid (int !j)) 
         [SPLICE; set (idx cache tid) temp];
      syncthreads
    ] in
    let loopPInputs = 
      [|idx cache $ add tid (int !j); idx cache tid |] in
    codeBuffer#splice_emit payload loopPInputs [|temp|] template; 
    j := !j / 2
  done;
  let buffer = DynArray.create () in 
  while !j > 0 do
    let payloadArgs = [|idx cache $ add tid (int !j); idx cache tid |] in
    let code = codeBuffer#splice payload payloadArgs [|temp|] [SPLICE] in 
    List.iter (fun stmt -> DynArray.add buffer stmt) code;
    DynArray.add buffer (setidx cache [tid] temp);
    j := !j / 2
  done;  
  codeBuffer#emit [ifTrue (lt tid (int 32)) (DynArray.to_list buffer)];
  codeBuffer#emit [ifTrue (eq tid (int 0))
                 [set (idx output linearBlockIdx) (idx cache $ int 0)]
               ];
  fnState#finalize
*)

(* PULLED from ImpGenReduce2D.ml 
(* Assumes 256 threads per block *)
let gen_reduce_2d payload inputType =
  let fnState = new imp_fnState in
  let elType = DynType.peel_vec inputType in
  let input = fnState#fresh_input inputType in
  let init = fnState#fresh_input elType in
  let output = fnState#fresh_output elType in
  let cache = fnState#shared_vec_var elType [256] in
  let idx = fnState#fresh_var Int32T in
  let idy = fnState#fresh_var Int32T in
  let vec_len = fnState#fresh_var Int32T in
  let firstvec = fnState#fresh_var Int32T in
  let elidx = fnState#fresh_var Int32T in
  let cacheid = fnState#fresh_var Int32T in
  let cur_y = fnState#fresh_var Int32T in
  codeBuffer#emit [
    set idx (add (mul blockIdx.x blockDim.x) threadIdx.x);
    set idy (add (mul blockIdx.y blockDim.y) threadIdx.y);
    set vec_len (idx input (int 0));
  ];
  let tmparg1 = fnState#fresh_var elType in
  let tmparg2 = fnState#fresh_var elType in
  let tmprslt = fnState#fresh_var elType in
  let template1 = [
    set firstvec (mul (int 2) idy);
      set elidx (add (mul firstvec vec_len) idx);
      set cacheid (add (mul blockDim.x threadIdx.y) threadIdx.x);
      setidx cache cacheid init;
      ifTrue (lt firstvec (len input)) [
      set tmparg1 (idx cache cacheid);
      set tmparg2 (idx input elidx);
      SPLICE;
        setidx cache cacheid tmprslt
      ]
  ] in
  let spliced1 = codeBuffer#splice
    payload [|tmparg1;tmparg2|] [|tmprslt|] template1 in
  let template2 = [                            
      ifTrue (lt (add firstvec (int 1)) (len input)) [
      set tmparg1 (idx cache cacheid);
      set tmparg2 (idx input (add elidx vec_len));
      SPLICE;
        setidx cache cacheid tmprslt
      ];
      syncthreads
  ] in
  let spliced2 = codeBuffer#splice
    payload [|tmparg1;tmparg2|] [|tmprslt|] template2 in 
  let template3 = [
      (* TODO: Support right-shift - div is expensive *)
      set cur_y (div blockDim.y (int 2));
      
      while_ (gt cur_y (int 0)) [
        ifTrue (lt threadIdx.y cur_y) [
        set tmparg1 (idx cache cacheid);
        set tmparg2 (idx cache (add cacheid (mul cur_y blockDim.x)));
        SPLICE;
          setidx cache cacheid tmprslt
        ]
        syncthreads;
        set cur_y (div cur_y (int 2));
      ]
    
    ifTrue ((threadIdx.y =$ 0) &&$ (threadIdx.x <$ blockDim.x)) [
      setidx output (add (mul vec_len blockIdx.y) idx) (idx cache threadIdx.x)
    ]
  ] in
  let spliced3 = codeBuffer#splice
    payload [|tmparg1;tmparg2|] [|tmprslt|] template3 in
  let bigIfBlock = spliced1 @ spliced2 @ spliced3 in
  let body = [ifTrue (lt idx vec_len) bigIfBlock] in
  codeBuffer#emit body;
  fnState#finalize
*)
