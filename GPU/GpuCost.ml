open Base
open SSA 


let safeFundefCache : (FnId.t, bool) Hashtbl.t = Hashtbl.create 127     


(* TIME IN MILLISECONDS-- ignore device->device copy costs for now  *) 
let rec transfer_time shape t = 
  (* assume 2MB / millisecond *) 
  let transferRate = 2097152 in
  let nbytes = Shape.nelts shape * DynType.sizeof t in 
  (* assume initiating a transfer requires at least 1 millisecond *)
  1 + nbytes / transferRate     

type gpu_data_info = Shape.t * DynType.t * bool

(* time to transfer a set of value to gpu, which are tagged with whether
   they're already on the gpu 
*) 
let rec sum_transfer_time = function 
  | [] -> 0 
  | (shape,t,onGpu)::rest -> 
      if onGpu then sum_transfer_time rest
      else transfer_time shape t + sum_transfer_time rest 
      
           
let map 
      ~(memState:MemoryState.t)
      ~(fnTable:FnTable.t)
      ~(fn:SSA.fundef)
      ~(closureArgs : gpu_data_info list)
      ~(dataArgs : gpu_data_info list) =  
    let launchCost = 3  in
    (* assume we can transfer 100,000 elements per millsecond to GPU, 
           and that allocation costs 3ms no matter the size 
         *)
    let mem_cost total (shape,t,onGpu) = 
      if onGpu then total else total + transfer_time shape t 
    in   
    let memoryCosts = 
      sum_transfer_time closureArgs + sum_transfer_time dataArgs
    in   
    let argShapes = List.map (fun (s,_,_) -> s) dataArgs in  
    let maxShape = match Shape.max_shape_list argShapes with 
      | Some maxShape -> maxShape
      | None -> failwith "no common shape found"
    in  
    (* assume each processor can process 1000 elements per millisecond, and 
       we have 100 processors-- what about cost of nested function? 
    *)
    let runCost = Shape.nelts maxShape / 100  in 
    launchCost + memoryCosts + runCost 
  
let reduce 
      ~(memState:MemoryState.t) 
      ~(fnTable:FnTable.t)
      ~(init:SSA.fundef)
      ~(initClosureArgs:InterpVal.t list)
      ~(fn:SSA.fundef)
      ~(closureArgs:InterpVal.t list)
      ~(initArgs:InterpVal.t list) 
      ~(args:InterpVal.t list) = 100 
          
let array_op memState op argVals = match op, argVals with 
  | _ -> 10