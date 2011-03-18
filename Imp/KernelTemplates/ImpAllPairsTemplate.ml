open Base
open Imp
open DynType

(* Assumes that inputs are 2D and that the payload is a function that takes *)
(* two input vectors and an output vector and populates the output vector *)
(* with the result of the function applied to the two inputs. *)
let gen_all_pairs_2d_naive payload t1 t2 outTypes =
  let outType = List.hd outTypes in
  let fnState = new ImpCodegen.fn_state in
  let input1 = fnState#fresh_input t1 in 
  let input2 = fnState#fresh_input t2 in
  let output = 
    fnState#fresh_output ~dims:[dim 0 input1; dim 0 input2] outType 
  in
  let left_id = fnState#fresh_var Int32T in
  let right_id = fnState#fresh_var Int32T in
  let threadsPerDim = 16 in
  let codeBuffer = fnState#main_code_buffer in 
  codeBuffer#emit [
    set left_id (threadIdx.x +$ (blockIdx.x *$ (int threadsPerDim)));
    set right_id (threadIdx.y +$ (blockIdx.y *$ (int threadsPerDim)))
  ];
  codeBuffer#splice_emit payload
    [|(idx input1 left_id);(idx input2 right_id)|]
    [|idx (idx output left_id) right_id|]
    [
      ifTrue (and_ (left_id <$ (len input1)) (right_id  <$ (len input2))) [
        SPLICE;
      ]
    ];
  fnState#finalize

(* Eric: Every thread is responsible for a single output element, the element
   being of dimensionality equal to that of the elements of the inputs.
   If, however, the all-pairs has a nested reduce, we'll want to have it
   generate elements of dimensionality one less than that of the input
   elements.

   I guess for now, I'm just going to implement the all-pairs case of 2D
   inputs and an embedded reduce resulting in a 2D output...
 *)
let gen_all_pairs_2d payload t1 t2 outTypes =
  let outType = List.hd outTypes in 
  debug $ Printf.sprintf
     "[imp] generating all_pairs kernel with signature (%s,%s)->%s\n"
     (DynType.to_str t1) (DynType.to_str t2) (DynType.to_str outType)
  ;
  let fnState = new ImpCodegen.fn_state in  
  let output_elt_t = DynType.peel_vec outType in 
  let input1 = fnState#fresh_input t1 in 
  let input2 =  fnState#fresh_input t2 in
  let output = fnState#fresh_output outType in
  let left_id = fnState#fresh_var UInt32T in 
  let right_id = fnState#fresh_var UInt32T in
  let left_idx = fnState#fresh_var UInt32T in 
  let right_idx = fnState#fresh_var UInt32T in 
  (* Is there some way to get this to be an input param? *)
  let vec_len = fnState#fresh_var UInt32T in
  let result = fnState#fresh_var output_elt_t in
  let threads_per_dim = 16 in
  let codeBuffer = fnState#main_code_buffer in 
  codeBuffer#emit [
    set left_id (threadIdx.y +$ (int threads_per_dim *$ blockIdx.y));
    set right_id (threadIdx.x +$  (int threads_per_dim *$ blockIdx.x));
    set vec_len (len input1);
  ];
  let payloadInputs = [|left_idx; right_idx; input1; input2|] in
  let trueBranch =
    codeBuffer#splice payload payloadInputs [|result|]   
    [
      set left_idx (vec_len *$ left_id);
      set right_idx (vec_len *$ right_id);
      SPLICE;
      setidx output[right_id +$ (left_id *$ (len input1))] result
    ]
  in 
  let mainCond = 
    (left_id <$ len input1) &&$ (right_id <$ len input2) 
  in
  codeBuffer#emit [ifTrue mainCond trueBranch];
  fnState#finalize

let gen_all_pairs_2d_tiled payload t1 t2 outType =
  let fnState = new ImpCodegen.fn_state in 
  let elt_t1, elt_t2 = DynType.peel_vec t1, DynType.peel_vec t2 in
  let output_elt_t = DynType.peel_vec outType in
  let input1 = fnState#fresh_input t1 in 
  let input2 = fnState#fresh_input t2 in
  let output = fnState#fresh_output outType in
  let i = fnState#fresh_var Int32T in 
  let j =  fnState#fresh_var Int32T in
  let result = fnState#fresh_var output_elt_t in
  let interm = fnState#fresh_var output_elt_t in
  let left_start_idx = fnState#fresh_var Int32T in 
  let right_start_idx = fnState#fresh_var Int32T in
  let threads_per_dim = 16 in
  let vec_len = fnState#fresh_var Int32T in
  let leftS = 
    fnState#shared_vec_var elt_t1 [threads_per_dim; threads_per_dim] 
  in
  let rightS = 
    fnState#shared_vec_var elt_t2 [threads_per_dim; threads_per_dim] 
  in
  let codeBuffer = fnState#fresh_code_buffer in 
  codeBuffer#emit [
    set vec_len (len (idx input1 (int 0)));
    set left_start_idx 
      (threadIdx.x +$ 
       (vec_len *$ (threadIdx.y  +$ blockIdx.y *$ int threads_per_dim)));
    set right_start_idx 
      (threadIdx.x +$  
        (vec_len *$  (threadIdx.y +$ blockIdx.x *$ int threads_per_dim)));
    set i (int 0)];
  let offset = fnState#fresh_var Int32T in
  codeBuffer#emit [
    while_ (i +$ (int threads_per_dim) <=$ vec_len) [
      setidx leftS[threadIdx.x; threadIdx.y] (idx input1 (i +$ left_start_idx));
	    setidx rightS[threadIdx.x; threadIdx.y]
        (idx input2 (i +$ right_start_idx));
	    syncthreads;
	    set j (int 0);
	    while_ (j <$ int threads_per_dim) [
	      set interm 
	       (idx (idx leftS j) threadIdx.y -$ idx (idx rightS j) threadIdx.x);
	      set interm (interm *$ interm);
	      set result (result +$ interm);
	      set j (j +$ (int 1))
      ];
	    syncthreads;
      
	    set i (i +$ int threads_per_dim)
    ];

    (* Code to handle non-multiple-of-threads-per-dim vec_lens *)
    set offset (int threads_per_dim *$ (vec_len /$ int threads_per_dim));
    ifTrue (offset <>$ vec_len) [
      ifTrue ((threadIdx.x +$ offset) <$ vec_len) [
        setidx leftS [threadIdx.x; threadIdx.y] 
          (idx input1 (left_start_idx +$ offset));
        setidx rightS [threadIdx.x; threadIdx.y]
          (idx input2 (right_start_idx +$ offset))
      ];
      syncthreads;
      
      set j (int 0);
      while_ (j <$ (vec_len -$ offset)) [
        set interm 
          ((idx (idx leftS j) threadIdx.y) -$ (idx (idx rightS j) threadIdx.x));
        set interm (interm *$ interm);
        set result (result +$ interm);
        set j (j +$ int 1)
      ];
      syncthreads
    ];
    setidx output 
      [threadIdx.y +$ blockIdx.y *$ blockDim.y; 
       threadIdx.x +$ blockIdx.x *$ blockDim.x]
      (sqrt32 result)
  ];
  fnState#finalize 
