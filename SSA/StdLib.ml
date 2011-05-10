open Base 
open SSA
open SSA_Codegen

(* 
   untyped representation of common Q functions we'd also like 
   to support in our system 
*)



let all = fn1 $ fun x y -> 
  [[y] :=  reduce @@ [scalar_op Prim.And; mk_bool true; x]]

let any = fn1 $ fun x y -> 
  [[y] := reduce @@ [scalar_op Prim.Or; mk_bool false; x]]      
  
let sum = fn1 $ fun x y -> 
  [[y] := reduce @@ [scalar_op Prim.Add; mk_int32 0; x]]

let prod = fn1 $ fun x y -> 
  [[y] := reduce @@ [scalar_op Prim.Mult; mk_int32 1; x]]

(* in Q, min and max are reductions, whereas | and & are the scalar versions *)
let min = fn1 $ fun x y -> 
  [[y] := reduce @@ [scalar_op Prim.Min; inf; x]]

let max = fn1 $ fun x y -> 
  [[y] := reduce @@ [scalar_op Prim.Max; neginf; x]] 

let count = fn1 $ fun x y -> 
  [[y] := mk_app (array_op Prim.DimSize) [x; mk_int32 0]]

let avg = mk_fn 1 1 2 $ fun inputs outputs locals -> [
    [locals.(0)] := reduce @@ [plus; mk_int32 0; inputs.(0)];
    [locals.(1)] := dimsize @@ [inputs.(0); mk_int32 0];  
    [outputs.(0)] := div @@ [locals.(0); locals.(1)]
  ]  


let initState = InterpState.create_from_untyped_list ~optimize:false [ 
  "any", any; 
  "all", all; 
  "sum", sum; 
  "prod", prod;
  
  "min_elt", min;
  "min", min;
  
  "max_elt", max;
  "max", max;
  
  "avg", avg;
  "mean", avg;
  
  "length", count;  
  "count", count 
]

let minidx_helper = mk_fn 4 2 1 $ fun inputs outputs locals ->
  let accIdx, accVal  = inputs.(0), inputs.(1) in
  let currIdx, currVal = inputs.(2), inputs.(3) in
  let keepAcc = locals.(0) in [
    [keepAcc] := lt @@ [accVal; currVal];    
    [outputs.(0)] := select @@ [keepAcc; accIdx; currIdx]; 
    [outputs.(1)] := select @@ [keepAcc; accVal; currVal]
  ]

let minidx = mk_fn 1 1 2 $ fun inputs outputs locals -> 
    let n, indices = locals.(0), locals.(1) in 
    let helper = 
      mk_globalfn (InterpState.get_untyped_id initState "$minidx_helper")
    in  
    [  
      [n] := dimsize @@ [inputs.(0)];
      (* TODO: will this run on the gpu? 
         It has a til operator built in! *)    
      [indices] := til @@ [n];
      (* TODO: what are the initial values? *)
      (* IDEA: Maybe need named argument groups? 
         ie: Reduce (fn, initvals = [i;j], data=[x;y;z]) 
      *) 
      [outputs.(0)] := 
         reduce @@ [helper; (* TODO: init val? *) indices; inputs.(0)]     
    ]    


(* K-means specific functions *)
(* calcCentroid [X;a;i] *) 
let calcCentroid = mk_fn 3 1 5 $ fun inputs outputs locals -> 
  let x = inputs.(0) in 
  let a = inputs.(1) in 
  let i = inputs.(2) in 
  [
    (* binVec: a = i *) 
    [locals.(0)] :=  map @@ [eq; a; i]; 
    (* idx: where binVec *) 
    [locals.(1)] := where @@ [locals.(0)]; 
    (* rows: X[idx] *) 
    [locals.(2)] := index @@ [x; locals.(1)]; 
    (* output: avg rows *) 
    [locals.(3)] := reduce @@ [plus; zero; locals.(2)];
    [locals.(4)] := (array_op Prim.DimSize) @@ [locals.(2); mk_int32 0];  
    [outputs.(0)] := (scalar_op Prim.Div) @@ [locals.(3); locals.(4)]
]
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "parakeet_calc_centroid" calcCentroid;;

(* calcCentroids[X;a;k] *) 
let calcCentroids = mk_fn 3 1 2 $ fun inputs outputs locals -> 
  let cc = 
    mk_globalfn $ 
      InterpState.get_untyped_id initState "parakeet_calc_centroid" 
  in
  let x = inputs.(0) in 
  let a = inputs.(1) in   
  let k = inputs.(2) in 
  [ 
    (* clusters: til k *) 
    [locals.(0)] := til @@ [k]; 
    (* calc_centroid[X;a] each clusters *)
    [locals.(1)] := cc @@ [x; a]; 
    [outputs.(0)] := map @@ [locals.(1); locals.(0)];
  ]
  ;;
           
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "parakeet_calc_centroids" calcCentroids;;

let dist_helper = mk_fn 3 1 2 $ fun inputs outputs locals -> 
  [ 
    [locals.(0)] := minus @@ [inputs.(1); inputs.(2)]; 
    [locals.(1)] := mul @@ [locals.(0); locals.(0)];
    [outputs.(0)] := plus @@ [inputs.(0); locals.(1)]
  ] 
let _ = 
  InterpState.add_untyped 
    initState 
    ~optimize:false 
    "parakeet_dist_helper" 
    dist_helper;;

let sqr_dist = mk_fn 2 1 0 $ fun inputs outputs _ -> 
  let dist_helper = 
    mk_globalfn (InterpState.get_untyped_id initState "parakeet_dist_helper") 
  in
  [
    [outputs.(0)] := reduce @@ [dist_helper; zero; inputs.(0); inputs.(1)];
  ] 
let _ = 
  InterpState.add_untyped 
    initState 
    ~optimize:false 
    "parakeet_sqr_dist" 
    sqr_dist;;
       

let dist = mk_fn  2 1 1 $ fun inputs outputs locals -> 
  let dist_helper = 
    mk_globalfn (InterpState.get_untyped_id initState "parakeet_dist_helper") 
  in
  [
    [locals.(0)] := reduce @@ [dist_helper; zero; inputs.(0); inputs.(1)];
    [outputs.(0)] := (scalar_op Prim.Sqrt) @@ [locals.(0)]
  ] 
let _ = InterpState.add_untyped initState ~optimize:false "parakeet_dist" dist;;

(* minidx[C;x] -> returns idx of whichever row of C is closest to x *)
(* 
     i = 0
     minIdx = -9999
     minDist = inf 
     n = len(C)
     while i < n
       c = c[i] 
       d = dist(c,x)
       if d < minDist
         minIdx = i 
         minDist = d
       i = i + 1 
     return minIdx
*) 
let minidx = mk_fn 2 1 15 $ fun inputs outputs locals ->
  let dist = mk_globalfn $ 
    InterpState.get_untyped_id initState "parakeet_sqr_dist" in
  let c = inputs.(0) in 
  let x = inputs.(1) in
  
  let n = locals.(0) in
  let test = locals.(1) in   
  
  let i_init = locals.(2) in 
  let i_bottom = locals.(3) in
  let i_top = locals.(4) in

  let minDist_init = locals.(5) in
  let minDist_update = locals.(6) in 
  let minDist_bottom = locals.(7) in
  let minDist_top = locals.(8) in
 
  let minIdx_init = locals.(9) in
  let minIdx_update = locals.(10) in 
  let minIdx_bottom = locals.(11) in
  let minIdx_top = outputs.(0) in
   
  let currRow = locals.(12) in 
  let currDist = locals.(13) in 
  let foundNewMin = locals.(14) in 
  let header = 
    SSA.mk_phi_nodes_from_values 
      [i_top; minDist_top; minIdx_top]
      [i_init; minDist_init; minIdx_init]
      [i_bottom; minDist_bottom; minIdx_bottom]
  in 
  let testBlock = Block.of_list [[test] := lt @@ [i_top; n]] in
  let newMinBlock = Block.of_list [
      [minDist_update] := value currDist;
      [minIdx_update] := value i_top;
  ]
  in
  let newMinPhi =
    SSA.mk_phi_nodes_from_values
      [minDist_bottom; minIdx_bottom]
      [minDist_update; minIdx_update]
      [minDist_top; minIdx_top]
  in
  let body = Block.of_list [
    [currRow] := index @@ [c; i_top]; 
    [currDist] := dist @@ [currRow; x];
    [foundNewMin] := lt @@ [currDist; minDist_top];
    SSA.mk_stmt $ SSA.If (foundNewMin, newMinBlock, Block.empty, newMinPhi); 
    [i_bottom] := plus @@ [i_top; one]
  ]
  in 
  [
    [i_init] := value zero;   
    [minDist_init] := value inf; 
    [minIdx_init] := value (SSA.mk_int32 (-9999));
    [n] := len c;
    SSA.mk_stmt $ SSA.WhileLoop(testBlock, test, body, header)
  ]    

let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "parakeet_minidx" minidx;;

(* takes as inputs X, number of clusters, and initial assignment *) 
let kmeans = mk_fn 3 1 2 $ fun inputs outputs locals ->
  let minIdx = 
    SSA.mk_globalfn (InterpState.get_untyped_id initState "parakeet_minidx") 
  in
  let calcCentroids = 
    SSA.mk_globalfn $ 
      InterpState.get_untyped_id initState "parakeet_calc_centroids"
  in 
  let x = inputs.(0) in 
  let a = inputs.(1) in 
  let k = inputs.(2) in  
  let c = locals.(0) in
  (*let newA = locals.(2) in*)  
  [
    (* C: calc_centroids[X;a;k]*)
    [c] := calcCentroids @@ [x;a;k];
    (* a: minidx[C] each X *)  
    [locals.(1)] := minIdx @@ [c]; 
    [outputs.(0)] := map @@ [locals.(1); x]; 
    (*[outputs.(0)] := calcCentroids @@ [x;newA;k];*)    
  ]
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "parakeet_kmeans" kmeans;;  
