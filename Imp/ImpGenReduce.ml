open Base 
open Imp
open ImpCodegen
open DynType

(* reduces each block's subvector to a single output element *) 
let gen_reduce payload threadsPerBlock outTypes =
  assert (List.length outTypes = 1); 
  let ty = List.hd outTypes in 
  debug (Printf.sprintf "type: %s" (DynType.to_str ty));
  let codegen = new imp_codegen in
  let input = codegen#fresh_input (VecT ty) in
  let output = codegen#fresh_output (VecT ty) in
  let cache = codegen#shared_vec_var ty [threadsPerBlock] in 
  let tid = codegen#fresh_var UInt32T in
  let i = codegen#fresh_var UInt32T in
  let linearBlockIdx = codegen#fresh_var UInt32T in
  let startBlock = codegen#fresh_var UInt32T in
  codegen#emit [
    set tid threadIdx.x;
    set linearBlockIdx (blockIdx.x +$ (blockIdx.y *$ gridDim.x));
    set startBlock ((int $ threadsPerBlock * 2) *$ linearBlockIdx);
    set i (threadIdx.x +$ startBlock)
  ];
  let temp = codegen#fresh_var ty in  
  let tmp1 = codegen#fresh_var ty in
  let tmp2 = codegen#fresh_var ty in
  
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
  
  let lenBlock = codegen#fresh_var UInt32T in
  debug "A";
  codegen#emit [
    set lenBlock (int $ threadsPerBlock * 2);
    ifTrue ((len input -$ startBlock) <$ lenBlock) [
      set lenBlock (len input -$ startBlock)
    ]
  ];
  let j = ref (threadsPerBlock / 2) in
  while !j >= 1 do
    debug (Printf.sprintf "B : %d" !j);
    let template = [
      ifTrue (tid <$ (int !j) &&$ (tid +$ (int !j) <$ lenBlock))
         [SPLICE; set (idx cache tid) temp];
      syncthreads
    ] in
        debug (Printf.sprintf "C : %d" !j);
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
