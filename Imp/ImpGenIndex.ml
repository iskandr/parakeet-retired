open Base
open DynType
open Imp
open ImpCodegen

(* Assumes: *)
(*   1. 256 threads per block *)
(*   2. "implicit" (VecT Int32T) indices input *) 
let gen_index inputType =
  let codegen = new imp_codegen in
  let input = codegen#fresh_input inputType in
  let indices = codegen#fresh_input (VecT Int32T) in
  let output = codegen#fresh_output inputType in
  let id = codegen#fresh_var Int32T in
  let idx = codegen#fresh_var Int32T in
  let offset = codegen#fresh_var Int32T in
  let vec = codegen#fresh_var Int32T in
  let vec_len = codegen#fresh_var Int32T in
  codegen#emit [
    set id (add (mul blockIdx.y (mul (int 16384) (int 256)))
                (add (mul blockIdx.x (int 256)) threadIdx.x));
    set vec_len (len (idx input (int 0)));
    ifTrue (lt id (mul (len indices) len (idx input (int 0)))) [
      set idx (div id vec_len);
      set offset (sub id (mul idx vec_len));
      set vec (idx indices idx);
      setidx output id (idx input (add (mul vec vec_len) offset));
    ]
  ];
  codegen#finalize
