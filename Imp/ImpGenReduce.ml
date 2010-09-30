open Base 
open Imp
open ImpCodegen
open DynType 

(* reduces each block's subvector to a single output element *) 
let gen_reduce payload threadsPerBlock ty =
  debug "[gen_reduce] start\n%";
  let codegen = new imp_codegen in 
  let input = codegen#fresh_var (VecT ty) in 
  let output = codegen#fresh_var (VecT ty) in 
  let cache = codegen#shared_vec_var ty [threadsPerBlock] in 
  let tid = codegen#fresh_var Int32T in 
  codegen#emit [set tid threadIdx.x]; 
  let i = codegen#fresh_var Int32T in 
  codegen#emit [set i (add Int32T threadIdx.x 
                        (mul Int32T blockIdx.x (int $ threadsPerBlock * 2)));
                set (idx cache tid) (idx input i)];
  let temp = codegen#fresh_var ty in
  let inputs = 
    [|idx input $ add Int32T i (int threadsPerBlock); idx cache tid |] in
  let template = [SPLICE; setidx cache [tid] temp] in 
  let trueBranch = codegen#splice payload inputs [|temp|] template in     
  let condition = 
    lt Int32T (add Int32T i (int threadsPerBlock)) (len input) in
  
  codegen#emit [ifTrue condition trueBranch; syncthreads];
  let j = ref 128 in 
  while !j >= 64 do
    let template = [
      ifTrue (lt Int32T tid (int !j)) 
         [SPLICE; set (idx cache tid) temp]; 
      syncthreads   
    ] in
    let loopPInputs = 
      [|idx cache $ add Int32T tid (int !j); idx cache tid |] in
    codegen#splice_emit payload loopPInputs [|temp|] template;   
    j := !j / 2
  done;
  let unrolledCodegen = new imp_codegen in 
  while !j > 0 do
    let loopPArgs = 
      [|idx cache $ add Int32T tid (int !j); idx cache tid |] in
    unrolledCodegen#splice_emit payload loopPArgs [|temp|] [SPLICE];
    j := !j / 2
  done;
  let unrolled = unrolledCodegen#finalize in 
  codegen#splice_emit unrolled [||] [||] 
    [ifTrue (lt Int32T tid (int 32)) [SPLICE]];
  codegen#emit [ifTrue (eq Int32T tid (int 0)) 
               [set (idx output blockIdx.x) (idx cache $ int 0 )]];
  debug "[gen_reduce] end\n%!";
  codegen#finalize