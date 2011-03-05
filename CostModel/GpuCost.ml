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
  
  let map 
        ~(fnTable:FnTable.t) 
        ~(fn:SSA.fundef) 
        ~(closureArgShapes : Shape.t list) 
        ~(argShapes : Shape.t list) =
    let symNestedCost = 0. in 
    let launchCost = 3.  in
    let maxShape = match Shape.max_shape_list argShapes with 
      | Some maxShape -> maxShape
      | None -> failwith "no common shape found"
    in  
    (* assume each processor can process 1000 elements per millisecond, and 
       we have 100 processors-- what about cost of nested function? 
    *)
    let parallelism = 100. in 
    let runCost = float_of_int (Shape.nelts maxShape) /. parallelism  in
    launchCost +.  runCost 
  
let reduce 
      ~(fnTable:FnTable.t)
      ~(init:SSA.fundef)
      ~(initClosureArgs:Shape.t list)
      ~(fn:SSA.fundef)
      ~(closureArgs:Shape.t list)
      ~(initArgs:Shape.t list) 
      ~(args:Shape.t list) = 100. 
          
let array_op op argShapes = 10.


