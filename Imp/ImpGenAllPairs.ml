open Base
open Imp
open ImpCodegen
open DynType 

(* Eric: Every thread is responsible for a single output element, the element
   being of dimensionality equal to that of the elements of the inputs.
   If, however, the all-pairs has a nested reduce, we'll want to have it
   generate elements of dimensionality one less than that of the input
   elements.

   I guess for now, I'm just going to implement the all-pairs case of 2D
   inputs and an embedded reduce resulting in a 2D output...
 *)
let gen_all_pairs_2d payload t1 t2 outType =
  debug $ Printf.sprintf
     "[imp] generating all_pairs kernel with signature (%s,%s)->%s\n"
     (DynType.to_str t1) (DynType.to_str t2) (DynType.to_str outType)
  ;
  let codegen = new imp_codegen in 
  (* change to actually get element types, or unwrap a vec *) 
  let elt_t1, elt_t2 = DynType.peel_vec t1, DynType.peel_vec t2 in 
  let output_elt_t = DynType.peel_vec outType in 
  let input1, input2 = codegen#fresh_input t1, codegen#fresh_input t2 in
  let output = codegen#fresh_output outType in
  let left_id, right_id = codegen#fresh_var Int32T, codegen#fresh_var Int32T in
  let left_idx = codegen#fresh_var Int32T in 
  let right_idx = codegen#fresh_var Int32T in 
  (* Is there some way to get this to be an input param? *)
  let vec_len = codegen#fresh_var Int32T in
  let result = codegen#fresh_var output_elt_t in
  let threads_per_dim = 16 in
  let mainCond =
    and_ (lt Int32T left_id (len input1))
         (lt Int32T right_id (len input2)) in
  codegen#emit [
    set left_id (add Int32T threadIdx.y
                (mul Int32T (int threads_per_dim) blockIdx.y));
    set right_id (add Int32T threadIdx.x
                   (mul Int32T (int threads_per_dim) blockIdx.x));
    set vec_len (len (idx input1 (int 0)))];
  let payloadInputs = [|left_idx; right_idx; input1; input2|] in
  let trueBranch =
    codegen#splice payload payloadInputs [|result|]   
    [
      set left_idx (mul Int32T vec_len left_id);
      set right_idx (mul Int32T vec_len right_id);
      SPLICE;
      setidx output 
        [add Int32T right_id  (mul Int32T left_id (len input1))] result
    ]
  in 
  codegen#emit [ifTrue mainCond trueBranch];
  codegen#finalize

let gen_all_pairs_2d_tiled payload t1 t2 outType =
  let codegen = new imp_codegen in
  let elt_t1, elt_t2 = DynType.peel_vec t1, DynType.peel_vec t2 in
  let output_elt_t = DynType.peel_vec outType in
  let input1, input2 = codegen#fresh_input t1, codegen#fresh_input t2 in
  let output = codegen#fresh_output outType in
  let i, j = codegen#fresh_var Int32T, codegen#fresh_var Int32T in
  let result = codegen#fresh_var output_elt_t in
  let interm = codegen#fresh_var output_elt_t in
  let left_start_idx, right_start_idx =
    codegen#fresh_var Int32T, codegen#fresh_var Int32T in
  let threads_per_dim = 16 in
  let vec_len = codegen#fresh_var Int32T in
  let leftS = codegen#shared_vec_var
    elt_t1 [threads_per_dim; threads_per_dim] in
  let rightS = codegen#shared_vec_var
    elt_t2 [threads_per_dim; threads_per_dim] in
  codegen#emit [
    set vec_len (len (idx input1 (int 0)));
    set left_start_idx (add Int32T threadIdx.x
      (mul Int32T vec_len
      (add Int32T threadIdx.y
      (mul Int32T blockIdx.y (int threads_per_dim)))));
    set right_start_idx (add Int32T threadIdx.x 
      (mul Int32T vec_len
      (add Int32T threadIdx.y
      (mul Int32T blockIdx.x (int threads_per_dim)))));
    set i (int 0)];
  let offset = codegen#fresh_var Int32T in
  codegen#emit [
    while_ (lte Int32T (add Int32T i (int threads_per_dim)) vec_len) [
      setidx leftS [threadIdx.x; threadIdx.y]
	      (idx input1 (add Int32T i left_start_idx));
	    setidx rightS [threadIdx.x; threadIdx.y]
	      (idx input2 (add Int32T i right_start_idx));
	    syncthreads;
      
	    set j (int 0);
	    while_ (lt Int32T j (int threads_per_dim)) [
	      set interm (sub output_elt_t
	       (idx (idx leftS j) threadIdx.y) (idx (idx rightS j) threadIdx.x));
	      set interm (mul output_elt_t interm interm);
	      set result (add output_elt_t result interm);
	      set j (add Int32T j (int 1))
      ];
	    syncthreads;
      
	    set i (add Int32T i (int threads_per_dim))
    ];

    (* Code to handle non-multiple-of-threads-per-dim vec_lens *)
    set offset (mul Int32T (int threads_per_dim)
                 (div Int32T vec_len (int threads_per_dim)));
    ifTrue (neq Int32T offset vec_len) [
      ifTrue (lt Int32T (add Int32T threadIdx.x offset) vec_len) [
        setidx leftS [threadIdx.x; threadIdx.y]
          (idx input1 (add Int32T left_start_idx offset));
        setidx rightS [threadIdx.x; threadIdx.y]
          (idx input2 (add Int32T right_start_idx offset))
      ];
      syncthreads;
      
      set j (int 0);
      while_ (lt Int32T j (sub Int32T vec_len offset)) [
        set interm (sub output_elt_t (idx (idx leftS j) threadIdx.y)
                                     (idx (idx rightS j) threadIdx.x));
        set interm (mul output_elt_t interm interm);
        set result (add output_elt_t result interm);
        set j (add Int32T j (int 1))
      ];
      syncthreads
    ];
    
    setidx output [(add Int32T threadIdx.y (mul Int32T blockIdx.y blockDim.y));
                   (add Int32T threadIdx.x (mul Int32T blockIdx.x blockDim.x))]
                  (sqrt32 result)
  ];
  codegen#finalize 
