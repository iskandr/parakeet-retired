open Base 
open Imp
open ImpCodegen
open DynType

(* reduces each block's subvector to a single output element *) 
let gen_reduce payload threadsPerBlock ty =
  debug (Printf.sprintf "type: %s" (DynType.to_str ty));
  let codegen = new imp_codegen in
  let input = codegen#fresh_input (VecT ty) in
  let output = codegen#fresh_output (VecT ty) in
  let cache = codegen#shared_vec_var ty [threadsPerBlock] in 
  let tid = codegen#fresh_var Int32T in
  let i = codegen#fresh_var Int32T in
  let linearBlockIdx = codegen#fresh_var Int32T in
  let startBlock = codegen#fresh_var Int32T in
  codegen#emit [
    set tid threadIdx.x;
    set linearBlockIdx (add Int32T blockIdx.x
                        (mul Int32T blockIdx.y gridDim.x));
    set startBlock (mul Int32T (int $ threadsPerBlock * 2) linearBlockIdx);
    set i (add Int32T threadIdx.x startBlock)
  ];
  
  let temp = codegen#fresh_var ty in  
  let tmp1 = codegen#fresh_var ty in
  let tmp2 = codegen#fresh_var ty in
  let template = [
    ifTrue (lt Int32T i (len input)) [
      if_ (lt Int32T (add Int32T i (int threadsPerBlock)) (len input)) [
        set tmp1 (idx input i);
        set tmp2 (idx input (add Int32T i (int threadsPerBlock)));
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
  codegen#emit [
    set lenBlock (int $ threadsPerBlock * 2);
    ifTrue (lt Int32T (sub Int32T (len input) startBlock) lenBlock) [
      set lenBlock (sub Int32T (len input) startBlock)
    ]
  ];
  let j = ref (threadsPerBlock / 2) in
  while !j >= 1 do
    let template = [
      ifTrue (and_ (lt Int32T tid (int !j))
                   (lt Int32T (add Int32T tid (int !j)) lenBlock))
         [SPLICE; set (idx cache tid) temp];
      syncthreads
    ] in
    let loopPInputs =
      [|idx cache $ add Int32T tid (int !j); idx cache tid|] in
    codegen#splice_emit payload loopPInputs [|temp|] template;
    j := !j / 2
  done;
  codegen#emit [ifTrue (eq Int32T tid (int 0))
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
    set linearBlockIdx (add Int32T blockIdx.x
                        (mul Int32T blockIdx.y gridDim.x));
    set i (add Int32T threadIdx.x
            (mul Int32T linearBlockIdx (int $ threadsPerBlock * 2)));
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
                 [set (idx output linearBlockIdx) (idx cache $ int 0)]
               ];
  codegen#finalize
