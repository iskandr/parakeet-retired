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
  "count", count; 
]
