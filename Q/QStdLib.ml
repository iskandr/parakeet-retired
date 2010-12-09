open Base 
open SSA
 
let reduce = mk_op  (Prim.ArrayOp Prim.Reduce) 
let map = mk_op (Prim.ArrayOp Prim.Map)
let inf = mk_num (PQNum.Inf DynType.Float32T)
let neginf = mk_num (PQNum.NegInf DynType.Float32T)

let (:=) xs y = mk_set (List.map SSA.get_id xs) y 
let (@@) fn args = mk_app fn args  
let scalar_op op = mk_op (Prim.ScalarOp op)
let array_op op = mk_op (Prim.ArrayOp op)

(* helper function for creating functions *) 
let fn nInputs nOutputs nLocals bodyConstructor = 
  let inputs = ID.gen_fresh_array nInputs in
  let inputVars = Array.map SSA.mk_var inputs in 
  let outputs = ID.gen_fresh_array nOutputs in
  let outputVars = Array.map SSA.mk_var outputs in
  let locals = ID.gen_fresh_array nLocals in 
  let localVars = Array.map SSA.mk_var locals in   
  let body = SSA.block_of_list $
    bodyConstructor inputVars outputVars localVars 
  in 
  mk_fundef 
    ~input_ids:(Array.to_list inputs)
    ~output_ids:(Array.to_list outputs)
    ~tenv:ID.Map.empty 
    ~body  

(* special case for creating function with 1 input, 1 output *) 
let fn1 constructor =
  let constructorWrapper = 
    fun inputs outputs _ -> constructor inputs.(0) outputs.(0)
  in 
  fn 1 1 0 constructorWrapper   

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

let avg = fn 1 1 2 $ fun inputs outputs locals -> [
    [locals.(0)] := reduce @@ [scalar_op Prim.Add; mk_int32 0; inputs.(0)];
    [locals.(1)] := mk_app (array_op Prim.DimSize) [inputs.(0); mk_int32 0];  
    [outputs.(0)] := mk_app (scalar_op Prim.Div) [locals.(0); locals.(1)]
  ]  
   

let initState = InterpState.create_from_untyped_list ~opt_queue:false [ 
  "any", any; 
  "all", all; 
  "sum", sum; 
  "prod", prod;
  "min", min;
  "max", max; 
  "avg", avg; 
  "count", count; 
]
