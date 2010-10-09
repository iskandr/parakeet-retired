open Base 
open Imp
open ImpCodegen
open DynType 

(* reduces each block's subvector to a single output element *) 
let gen_reduce payload threadsPerBlock ty =
  let codegen = new imp_codegen in
  let input = codegen#fresh_input (VecT ty) in
  let output = codegen#fresh_output (VecT ty) in
  let cache = codegen#shared_vec_var ty [threadsPerBlock] in 
  let tid = codegen#fresh_var Int32T in 
  codegen#emit [
    set tid threadIdx.x
  ];
  let i = codegen#fresh_var Int32T in 
  codegen#emit [
    set i (add Int32T threadIdx.x
            (mul Int32T blockIdx.x (int $ threadsPerBlock * 2)));
    set (idx cache tid) (idx input i)
  ];
  let temp = codegen#fresh_var ty in
  let inputs =
    [|idx input $ add Int32T i (int threadsPerBlock); idx cache tid |] in
  let template = [SPLICE; setidx cache [tid] temp] in
  let trueBranch = codegen#splice payload inputs [|temp|] template in
  let condition =
    lt Int32T (add Int32T i (int threadsPerBlock)) (len input) in
  
  codegen#emit [ifTrue condition trueBranch; syncthreads];
  let j = ref (threadsPerBlock / 2) in 
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
  let buffer = DynArray.create () in 
  while !j > 0 do 
    let payloadArgs = [|idx cache $ add Int32T tid (int !j); idx cache tid |] in
    let code = codegen#splice payload payloadArgs [|temp|] [SPLICE] in 
    List.iter (fun stmt -> DynArray.add buffer stmt) code; 
    DynArray.add buffer (setidx cache [tid] temp);
    j := !j / 2             
  done;  
  codegen#emit [ifTrue (lt Int32T tid (int 32)) (DynArray.to_list buffer)]; 
  codegen#emit [ifTrue (eq Int32T tid (int 0))
                 [set (idx output blockIdx.x) (idx cache $ int 0)]
               ];
  codegen#finalize