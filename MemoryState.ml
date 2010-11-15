open Base 
open Printf

type 'a generic_mem_state = {
  gpu_vals : ('a, GpuVal.gpu_val) Hashtbl.t;
  host_vals : ('a, HostVal.host_val) Hashtbl.t;
}

type mem_state = InterpVal.DataId.t generic_mem_state

let create numvars = {
  gpu_vals = Hashtbl.create (2*numvars);
  host_vals = Hashtbl.create (2*numvars); 
}

(*
let is_on_gpu state = function
  | InterpVal.Data id -> Hashtbl.mem state.gpu_vals id
  | _ -> failwith "Can't query location of non-data InterpVal"

let is_on_host state = function
  | InterpVal.Data id -> Hashtbl.mem state.host_vals id
  | _ -> failwith "Can't query location of non-data InterpVal"

let get_shape state = function
  | InterpVal.Data id ->
    if is_on_gpu state id then
      let gpuVal = get_gpu state id in
      GpuVal.get_shape gpuVal
    else
      let hostVal = get_host state id in
      HostVal.get_shape hostVal
  | _ -> "Can't get shape of non-data InterpVal"
*)

let add_host state hostVal = 
  let id = InterpVal.DataId.gen() in 
  Hashtbl.replace state.host_vals id hostVal; 
  InterpVal.Data id 
  
let add_gpu state gpuVal = 
  let id = InterpVal.DataId.gen() in 
  Hashtbl.replace state.gpu_vals id gpuVal; 
  InterpVal.Data id 

let get_gpu state = function 
  | InterpVal.Data id -> 
    if Hashtbl.mem state.gpu_vals id then
      Hashtbl.find state.gpu_vals id
    else (
      let hostVal = Hashtbl.find state.host_vals id in
      IFDEF DEBUG THEN Printf.printf "%s\n" (HostVal.to_str hostVal); ENDIF; 
      let gpuVal = GpuVal.to_gpu hostVal in
      Hashtbl.replace state.gpu_vals id gpuVal; 
      gpuVal
   )
  | InterpVal.Scalar n -> GpuVal.GpuScalar n  
  | InterpVal.Closure _ -> 
      failwith "[MemoryState->get_gpu] can't send function to gpu"

let get_host state = function 
  | InterpVal.Data id -> 
    if Hashtbl.mem state.host_vals id then
      Hashtbl.find state.host_vals id
    else
      let gpuVal = Hashtbl.find state.gpu_vals id in
      let hostVal = GpuVal.from_gpu gpuVal in
      Hashtbl.replace state.host_vals id hostVal;
      Printf.printf "[MemoryState->get_host] Got %s \n" 
        (HostVal.to_str hostVal);  
      hostVal
  | InterpVal.Scalar n -> HostVal.HostScalar n 
  | InterpVal.Closure _ -> 
      failwith "[MemoryState->get_host] can't send function to host memory"

let free_gpu state id =
  if Hashtbl.mem state.gpu_vals id then begin
    GpuVal.free (Hashtbl.find state.gpu_vals id);
    Hashtbl.remove state.host_vals id
  end

let free_host state id =
  if Hashtbl.mem state.host_vals id then begin
    GpuVal.free (Hashtbl.find state.gpu_vals id);
    Hashtbl.remove state.host_vals id
  end

(* free identifier on both host and gpu *) 
let free state id = free_gpu state id; free_host state id

let free_all_gpu state =
  Hashtbl.iter (fun _ gpuVal -> GpuVal.free gpuVal) state.gpu_vals;
  Hashtbl.clear state.gpu_vals