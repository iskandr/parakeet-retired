(* pp: -parser o pa_macro.cmo *)

open Base
open Cuda
open CudaModule
open Printf

(*
    Here we do the following:
    1. Query how many devices there are
    2. Query the devices' parameters
    3. Build a model of the available system resources
    4. Create device contexts for each device
*)

(* The number of devices, and the array which stores their info *)

let device_info = DynArray.create ()
(*
let device_contexts = DynArray.create ()
*)
let inited = ref false

let hw_init () =
  if !inited = false then begin
    inited := true;
    cuda_init_runtime ();
    let free, total = cuda_device_get_free_and_total_mem() in
    IFDEF DEBUG THEN
	    printf "Free and total memory: %d, %d\n" free total;
    ENDIF;
    (* TODO: For now, we'll just handle a single device, and handle *)
    (* contexts by using the one created by the runtime API *)
    let ndevices = cuda_device_get_count () in
    for i = 0 to ndevices - 1 do
      let properties = cuda_device_get_properties i in
      DynArray.add device_info properties;
      (*
      let ctx = cuda_ctx_create i in
      DynArray.add device_contexts ctx;
      *)
      IFDEF DEBUG THEN
        printf "Properties of GPU device %d: \n" i; 
        printf "\t -- max threads per block: %d\n" 
          properties.max_threads_per_block;
        printf "\t -- max blocks per grid x: %d\n" 
              properties.max_blocks_per_grid_x;
        printf "\t -- shared mem per block: %d\n"  
          properties.shared_mem_per_block;
        printf 
          "\t -- total constant mem: %d\n" properties.total_constant_mem; 
        printf "\t -- warp size: %d\n" properties.warp_size; 
        printf "\t -- clock_rate: %d\n" properties.clock_rate_khz;
     ENDIF;
    done
  end;
  ()

let safe_div x y = (x + y - 1) / y

let get_grid_params ?(device=0) ?(block_size=256) nThreads =
  let deviceInfo = DynArray.get device_info device in  
  let max_gridx = deviceInfo.max_blocks_per_grid_x in
  let max_gridy = deviceInfo.max_blocks_per_grid_y in
  if nThreads <= max_gridx * max_gridy * block_size then
    let nblocks = safe_div nThreads block_size in
    let x = min nblocks max_gridx in
    let y = safe_div nblocks max_gridx in  
    Some {threads_x=block_size; threads_y=1; threads_z=1;
          grid_x=x; grid_y=y}
  else None

let get_linear_grid_params ?(device=0) ?(block_size=256) nEls =
  let deviceInfo = DynArray.get device_info device in
  let max_gridx = deviceInfo.max_blocks_per_grid_x in
  let max_gridy = deviceInfo.max_blocks_per_grid_y in
  let num_blocks = safe_div nEls block_size in
  if num_blocks <= max_gridx then
    {threads_x=block_size; threads_y=1; threads_z=1;
     grid_x=num_blocks; grid_y=1}
  else
    {threads_x=block_size; threads_y=1; threads_z=1;
     grid_x=max_gridx; grid_y=safe_div num_blocks max_gridx}
