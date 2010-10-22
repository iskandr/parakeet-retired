open Base 
open Printf

open DynVal 

type 'a mem_state = {
  gpu_vals : ('a, GpuVal.gpu_val) Hashtbl.t;
  host_vals : ('a, HostVal.host_val) Hashtbl.t;
}

let create numvars = {
  gpu_vals = Hashtbl.create (2*numvars);
  host_vals = Hashtbl.create (2*numvars); 
}

let add_host state hostVal = 
  let id = DataId.gen() in 
  Hashtbl.replace state.host_vals id hostVal; 
  Data id 
  
let add_gpu state gpuVal = 
  let id = DataId.gen() in 
  Hashtbl.replace state.gpu_vals id gpuVal; 
  Data id 

let get_gpu state = function 
  | Scalar n -> GpuVal.mk_scalar n 
  | Data id -> 
    if Hashtbl.mem state.gpu_vals id then
      Hashtbl.find state.gpu_vals id
    else (
      let hostVal = Hashtbl.find state.host_vals id in
      debug $ HostVal.to_str hostVal; 
      let gpuVal = GpuVal.to_gpu hostVal in
      Hashtbl.replace state.gpu_vals id gpuVal; 
      gpuVal
   )

let get_host state = function 
  | Scalar n -> HostVal.mk_scalar n 
  | Data id -> 
    if Hashtbl.mem state.host_vals id then
      Hashtbl.find state.host_vals id
    else
      let gpuVal = Hashtbl.find state.gpu_vals id in
      let hostVal = GpuVal.from_gpu gpuVal in
        Hashtbl.replace state.host_vals id hostVal; 
      hostVal

  
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
