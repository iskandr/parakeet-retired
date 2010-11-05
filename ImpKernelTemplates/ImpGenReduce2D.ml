open Base
open DynType
open Imp
open ImpCodegen

(* Assumes 256 threads per block *)
let gen_reduce_2d payload inputType =
  let codegen = new imp_codegen in
  let elType = DynType.peel_vec inputType in
  let input = codegen#fresh_input inputType in
  let init = codegen#fresh_input elType in
  let output = codegen#fresh_output elType in
  let cache = codegen#shared_vec_var elType [256] in
  let idx = codegen#fresh_var Int32T in
  let idy = codegen#fresh_var Int32T in
  let vec_len = codegen#fresh_var Int32T in
  let firstvec = codegen#fresh_var Int32T in
  let elidx = codegen#fresh_var Int32T in
  let cacheid = codegen#fresh_var Int32T in
  let cur_y = codegen#fresh_var Int32T in
  codegen#emit [
    set idx (add (mul blockIdx.x blockDim.x) threadIdx.x);
    set idy (add (mul blockIdx.y blockDim.y) threadIdx.y);
    set vec_len (idx input (int 0));
  ];
  let tmparg1 = codegen#fresh_var elType in
  let tmparg2 = codegen#fresh_var elType in
  let tmprslt = codegen#fresh_var elType in
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
  let spliced1 = codegen#splice
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
  let spliced2 = codegen#splice
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
  let spliced3 = codegen#splice
    payload [|tmparg1;tmparg2|] [|tmprslt|] template3 in
  let bigIfBlock = spliced1 @ spliced2 @ spliced3 in
  let body = [ifTrue (lt idx vec_len) bigIfBlock] in
  codegen#emit body;
  codegen#finalize
