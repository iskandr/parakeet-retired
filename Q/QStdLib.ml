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
    [locals.(0)] := reduce @@ [scalar_op Prim.Add; mk_int32 0; inputs.(0)];
    [locals.(1)] := mk_app (array_op Prim.DimSize) [inputs.(0); mk_int32 0];  
    [outputs.(0)] := mk_app (scalar_op Prim.Div) [locals.(0); locals.(1)]
  ]  


let initState = InterpState.create_from_untyped_list ~optimize:false [ 
  "any", any; 
  "all", all; 
  "sum", sum; 
  "prod", prod;
  "min", min;
  "max", max; 
  "avg", avg; 
  "count", count 
]

(* K-means specific functions *)
let calcCentroid = mk_fn 3 1 5 $ fun inputs outputs locals -> [
    (* binVec: a = i *) 
    [locals.(0)] :=  map @@ [eq; inputs.(1); inputs.(2)]; 
    (* idx: where binVec *) 
    [locals.(1)] := where @@ [locals.(0)]; 
    (* rows: X[idx] *) 
    [locals.(2)] := index @@ [inputs.(0); locals.(1)]; 
    (* output: avg rows *) 
    [locals.(3)] := reduce @@ [plus; zero; locals.(2)];
    [locals.(4)] := (array_op Prim.DimSize) @@ [locals.(2); mk_int32 0];  
    [outputs.(0)] := (scalar_op Prim.Div) @@ [locals.(3); locals.(4)]
]
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "calc_centroid" calcCentroid;;


let calcCentroids = mk_fn 3 1 2 $ fun inputs outputs locals -> 
  let cc = mk_globalfn (InterpState.get_untyped_id initState "calc_centroid") in 
  [ 
    (* clusters: til k *) 
    [locals.(0)] := til @@ [inputs.(2)]; 
    (* calc_centroid[X;a] each clusters *)
    [locals.(1)] := cc @@ [inputs.(0); inputs.(1)]; 
    [outputs.(0)] := map @@ [locals.(1); locals.(0)]
  ]     
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "calc_centroids" calcCentroids;;

let dist = mk_fn  2 1 3 $ fun inputs outputs locals -> 
  [
    [locals.(0)] := minus @@ [inputs.(0); inputs.(1)]; 
    [locals.(1)] := mul @@ [locals.(0); locals.(0)]; 
    [locals.(2)] := reduce @@ [plus; zero; locals.(1)];
    [outputs.(0)] := (scalar_op Prim.Sqrt) @@ [locals.(2)]
  ] 
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "dist" dist;;

(* minidx[C;x] -> returns idx of whichever row of C is closest to x *) 
let minidx = mk_fn 2 1 3 $ fun inputs outputs locals ->
  let dist = mk_globalfn $ InterpState.get_untyped_id initState "dist" in  
  let min = mk_globalfn $ InterpState.get_untyped_id initState "min" in  
  [ 
    (* ds: map dist[x] C *) 
    [locals.(0)] := dist @@ [inputs.(1)];
    [locals.(1)] := map @@ [locals.(0); inputs.(0)];
    (* m: min ds *)  
    [locals.(2)] := min @@ [locals.(1)];
    (* midx: find m ds *) 
    [outputs.(0)] := find @@ [locals.(1); locals.(2)]
  ]    
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "minidx" minidx;;

(* takes as inputs X, number of clusters, and initial assignment *) 
let kmeans = mk_fn 3 1 2 $ fun inputs outputs locals ->
  let minIdx = 
    SSA.mk_globalfn (InterpState.get_untyped_id initState "minidx") 
  in
  let calcCentroids = 
    SSA.mk_globalfn $ InterpState.get_untyped_id initState "calc_centroids"
  in 
  (*let countVar = SSA.mk_var $ InterpState.get_untyped_id initState "count" in*)
  [
    (* C: calc_centroids[X;a;k]*)
    [locals.(0)] := calcCentroids @@ [inputs.(0); inputs.(2); inputs.(1)];
    (* a: minidx[C] each X *)  
    [locals.(1)] := minIdx @@ [locals.(0)]; 
    [outputs.(0)] := map @@ [locals.(1); inputs.(0)]   
  ]
let _ = 
  InterpState.add_untyped 
    initState ~optimize:false "kmeans" kmeans;;  
