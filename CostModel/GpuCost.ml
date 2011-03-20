(* pp: -parser o pa_macro.cmo *)

open Base
open SSA 


let safeFundefCache : (FnId.t, bool) Hashtbl.t = Hashtbl.create 127     


(* TIME IN MILLISECONDS-- ignore device->device copy costs for now  *) 
let rec transfer_time shape t : float = 
  (* assume 2MB / millisecond *) 
  let transferRate = 2097152. in
  let nbytes = Shape.nelts shape * DynType.sizeof t in 
  (* assume initiating a transfer requires at least 1 millisecond *)
  1. +. (float_of_int nbytes) /. transferRate     

type gpu_data_info = Shape.t * DynType.t * bool

(* time to transfer a set of value to gpu, which are tagged with whether
   they're already on the gpu 
*) 
let rec sum_transfer_time = function 
  | [] -> 0.
  | (t,shape,onGpu)::rest -> 
      if onGpu then sum_transfer_time rest
      else transfer_time shape t +. sum_transfer_time rest 
  
  (* constants useful for computing adverb costs on the GPU *) 
  let parallelism = 240. 
  let launchCost = 3. 
    
  let map 
        ~(fnTable:FnTable.t) 
        ~(fn:SSA.fundef) 
        ~(closureArgs : Shape.t list) 
        ~(args : Shape.t list) =
    let outerDim, nestedArgs = Shape.split_nested_shapes args in
    let nestedShapes = closureArgs @ nestedArgs in 
    let nestedCost = SeqCost.seq_cost fnTable fn nestedShapes in 
    let runCost = (float_of_int outerDim) *. nestedCost /. parallelism in
    IFDEF DEBUG THEN 
      Printf.printf "[GpuCost] Computing GPU map cost for %s, %s\n"
        (String.concat ", " (List.map Shape.to_str closureArgs))
        (String.concat ", " (List.map Shape.to_str args))
      ;
      Printf.printf "[GpuCost] nestedArgs: {%s}, nestedCost: %f, runCost: %f\n"
        (String.concat ", " (List.map Shape.to_str nestedShapes))
        nestedCost
        runCost
      ; 
    ENDIF; 
    launchCost +.  runCost 
  
let reduce 
      ~(fnTable:FnTable.t)
      ~(init:SSA.fundef)
      ~(initClosureArgs:Shape.t list)
      ~(fn:SSA.fundef)
      ~(closureArgs:Shape.t list)
      ~(initArgs:Shape.t list) 
      ~(args:Shape.t list) =
    (* for now, assume the initFn takes as much time as the normal fn *)
    let outerDim, nestedArgs = Shape.split_nested_shapes args in
    let nestedShapes = initClosureArgs @ initArgs @ nestedArgs in
    let nestedCost = SeqCost.seq_cost fnTable init nestedShapes in
    let runCost = (float_of_int outerDim) *. nestedCost /. parallelism in 
    let numLaunches = log (float_of_int outerDim) in  
    numLaunches *. launchCost +.  2. *. runCost                
          
let array_op op argShapes = match op, argShapes with 
  | Prim.Where, [x] -> float_of_int  (Shape.nelts x)
  | Prim.Index, [x;idx] -> 
    (* assume x is 1D *) 
    let numIndices = Shape.nelts idx in
    float_of_int numIndices   
  | Prim.Find, [x; elt] -> float_of_int $ (Shape.nelts x) * (Shape.nelts elt) 
  | arrayOp, shapes -> 
      IFDEF DEBUG THEN
        Printf.printf "[GpuCost] Infinite cost for operator: %s with args %s\n" 
          (Prim.array_op_to_str arrayOp)  
          (String.concat ", " (List.map Shape.to_str shapes))  
      ENDIF; 
      infinity 

