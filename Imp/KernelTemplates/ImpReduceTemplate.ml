(* pp: -parser o pa_macro.cmo *)

open Base
open DynType
open Imp
open ImpCodegen

let gen_reduce_2d_capable payload threadsPerBlock =
  (* For now only works on 1D and 2D inputs *)
  let codegen = new imp_codegen in
  IFDEF DEBUG THEN 
    assert (Array.length payload.input_types = 2);
    assert (Array.length payload.output_types = 1);
  ENDIF; 
  (* assume result of reduction is same as elements of vector *) 
  let ty = payload.output_types.(0) in 
  let input = codegen#fresh_input (VecT ty) in
  let eltype = DynType.elt_type ty in
  IFDEF DEBUG THEN
    Printf.printf 
      "Creating reduce kernel with output types %s with payload (%s) -> (%s)\n"
      (DynType.type_array_to_str payload.input_types)
      (DynType.type_array_to_str payload.output_types);
  ENDIF;  
  let head::tail = SymbolicShape.all_dims input in
  let outputDims = (safe_div_ head (int $ threadsPerBlock * 2))::tail in 
  let output = codegen#fresh_output ~dims:outputDims  (VecT ty) in

  let cache = codegen#shared_vec_var eltype [threadsPerBlock] in
  
  let num_vecs = codegen#fresh_var Int32T in
  codegen#emit [set num_vecs (len input)];
  
  let vec_len = codegen#fresh_var Int32T in
  let ndims = DynType.nest_depth (VecT ty) in
  if ndims = 1 then
    codegen#emit [set vec_len (int 1)]
  else if ndims = 2 then
    codegen#emit [set vec_len (dim 2 input)]
  else
    failwith "Reduce only supported on 1D or 2D inputs for now";
  
  let spin1 = codegen#fresh_var eltype in
  let spin2 = codegen#fresh_var eltype in
  let spout = codegen#fresh_var eltype in
  
  let bx = codegen#fresh_var Int32T in
  let by = codegen#fresh_var Int32T in
  codegen#emit [
    comment "bx = blockDim.x"; 
    set bx blockDim.x;
    comment "by = blockDim.y";
    set by blockDim.y
  ];
  
  let id_x = codegen#fresh_var Int32T in
  let id_y = codegen#fresh_var Int32T in
  codegen#emit [
    comment "idx_x = blockIdx.x * bx + threadIdx.x";
    set id_x ((blockIdx.x *$ bx) +$ threadIdx.x);
    comment "id_y = blockIdx.y * by + threadIdx.y";
    set id_y ((blockIdx.y *$ by) +$ threadIdx.y)
  ];
  
  let firstvec = codegen#fresh_var Int32T in
  let cacheid = codegen#fresh_var Int32T in
  let cur_y = codegen#fresh_var Int32T in
  
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
  codegen#splice_emit payload [|spin1;spin2|] [|spout|] template;
  codegen#finalize

(* reduces each block's subvector to a single output element *) 
let gen_reduce_old payload threadsPerBlock =
  assert (Array.length payload.output_types = 2);
  (* assume result of reduction is same as elements of vector *) 
  let ty = payload.output_types.(1) in
  let codegen = new imp_codegen in
  let input = codegen#fresh_input (VecT ty) in
  let eltype = DynType.elt_type ty in
  let head::tail = SymbolicShape.all_dims input in
  let outputDims =  ((safe_div_ head (int $ threadsPerBlock * 2))::tail) in 
  let output = codegen#fresh_output ~dims:outputDims (VecT ty) in

  let cache = codegen#shared_vec_var eltype [threadsPerBlock] in

  let tid = codegen#fresh_var UInt32T in
  let linearBlockIdx = codegen#fresh_var UInt32T in
  let startBlock = codegen#fresh_var UInt32T in
  let i = codegen#fresh_var UInt32T in
  codegen#emit [
    set tid threadIdx.x;
    set linearBlockIdx (blockIdx.x +$ (blockIdx.y *$ gridDim.x));
    set startBlock ((int $ threadsPerBlock * 2) *$ linearBlockIdx);
    set i (threadIdx.x +$ startBlock)
  ];
  
  let temp = codegen#fresh_var eltype in
  let tmp1 = codegen#fresh_var eltype in
  let tmp2 = codegen#fresh_var eltype in
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
  codegen#splice_emit payload [|tmp1;tmp2|] [|temp|] template;
  
  let lenBlock = codegen#fresh_var Int32T in
  let tmpInt = codegen#fresh_var Int32T in
  codegen#emit [
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
    codegen#splice_emit payload loopPInputs [|temp|] template;
    j := !j / 2
  done;
  
  codegen#emit [ifTrue (tid =$ (int 0))
                 [set (idx output linearBlockIdx) (idx cache $ int 0)]
               ];
  codegen#finalize

(* reduces each block's subvector to a single output element *) 
let gen_reduce_opt payload threadsPerBlock ty =
  let codegen = new imp_codegen in
  let input = codegen#fresh_input (VecT ty) in
  let output = codegen#fresh_output (VecT ty) in
  let cache = codegen#shared_vec_var ty [threadsPerBlock] in 
  let tid = codegen#fresh_var Int32T in 
  codegen#emit [
    set tid threadIdx.x
  ];
  let i = codegen#fresh_var Int32T in
  let linearBlockIdx = codegen#fresh_var Int32T in
  codegen#emit [
    set linearBlockIdx (add blockIdx.x
                        (mul blockIdx.y gridDim.x));
    set i (add threadIdx.x
            (mul linearBlockIdx (int $ threadsPerBlock * 2)));
    set (idx cache tid) (idx input i)
  ];
  let temp = codegen#fresh_var ty in
  let inputs =
    [|idx input $ add i (int threadsPerBlock); idx cache tid |] in
  let template = [SPLICE; setidx cache [tid] temp] in
  let trueBranch = codegen#splice payload inputs [|temp|] template in
  let condition =
    lt (add i (int threadsPerBlock)) (len input) in
  
  codegen#emit [ifTrue condition trueBranch; syncthreads];
  let j = ref (threadsPerBlock / 2) in 
  while !j >= 64 do
    let template = [
      ifTrue (lt tid (int !j)) 
         [SPLICE; set (idx cache tid) temp];
      syncthreads
    ] in
    let loopPInputs = 
      [|idx cache $ add tid (int !j); idx cache tid |] in
    codegen#splice_emit payload loopPInputs [|temp|] template; 
    j := !j / 2
  done;
  let buffer = DynArray.create () in 
  while !j > 0 do
    let payloadArgs = [|idx cache $ add tid (int !j); idx cache tid |] in
    let code = codegen#splice payload payloadArgs [|temp|] [SPLICE] in 
    List.iter (fun stmt -> DynArray.add buffer stmt) code;
    DynArray.add buffer (setidx cache [tid] temp);
    j := !j / 2
  done;  
  codegen#emit [ifTrue (lt tid (int 32)) (DynArray.to_list buffer)];
  codegen#emit [ifTrue (eq tid (int 0))
                 [set (idx output linearBlockIdx) (idx cache $ int 0)]
               ];
  codegen#finalize
