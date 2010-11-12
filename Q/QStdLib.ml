open Base 
open SSA
 
let reduce = mk_op  (Prim.ArrayOp Prim.Reduce) 
let map = mk_op (Prim.ArrayOp Prim.Map)
let (:=) xs y = mk_set (List.map SSA.get_id xs) y 
let (@@) fn args = mk_app fn args  
let scalar_op op = mk_op (Prim.ScalarOp op)
let array_op op = mk_op (Prim.ArrayOp op)

(* helper function for creating functions *) 
let fn nInputs nOutputs bodyConstructor = 
  let inputs = ID.gen_fresh_array nInputs in
  let inputVars = Array.map SSA.mk_var inputs in 
  let outputs = ID.gen_fresh_array nOutputs in
  let outputVars = Array.map SSA.mk_var outputs in 
  let body = bodyConstructor inputVars outputVars in 
  mk_fundef 
    ~input_ids:(Array.to_list inputs)
    ~output_ids:(Array.to_list outputs)
    ~tenv:ID.Map.empty 
    ~body  

(* special case for creating function with 1 input, 1 output *) 
let fn1 constructor =
  let constructorWrapper = 
    fun inputs outputs -> constructor inputs.(0) outputs.(0)
  in 
  fn 1 1 constructorWrapper   

(* 
   untyped representation of common Q functions we'd also like 
   to support in our system 
*)

let all = fn1 $ fun x y -> [
  [y] :=  reduce @@ [scalar_op Prim.And; mk_bool true; x]]

let any = fn1 $ fun x y -> [
  [y] := reduce @@ [scalar_op Prim.Or; mk_bool false; x]]      
  
let sum = fn1 $ fun x y -> [ 
  [y] := reduce @@ [scalar_op Prim.Add; mk_int32 0; x]]

let prod = fn1 $ fun x y -> [ 
  [y] := reduce @@ [scalar_op Prim.Mult; mk_int32 1; x]]

let initState = InterpState.create_from_untyped_list ~opt_queue:false [ 
  "any", any; 
  "all", all; 
  "sum", sum; 
  "prod", prod;
]
