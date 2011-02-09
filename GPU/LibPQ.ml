open Printf 
open Cuda 

module TimeSpecPtr = Int64

type grid_params = {
  threads_x : int;
  threads_y : int;
  threads_z : int;
  grid_x    : int;
  grid_y    : int;
}

type gpu_arg = GpuScalarArg of PQNum.num | GpuArrayArg of Int64.t * int

let gpu_arg_to_str = function 
  | GpuScalarArg n -> "Scalar: " ^ (PQNum.num_to_str n)
  | GpuArrayArg (ptr, n) -> Printf.sprintf "Array: %Lx (%d)" ptr n 

external cuda_init : unit -> unit = "ocaml_pq_init"

(* arguments: a ptx string and a desired number of threads per block *) 
external compile_module_impl
    : string -> int -> CuModulePtr.t = "ocaml_pq_compile_module"
let compile_module ptx threadsPerBlock =
  Timing.start_timer "Compile PTX";
  let modulePtr = compile_module_impl ptx threadsPerBlock in
  Timing.stop_timer "Compile PTX";
  modulePtr

external destroy_module
    : CuModulePtr.t -> unit = "ocaml_pq_destroy_module"

(* bytecode handles arity > 5 differently from natively compiled OCaml, *)
(* so need two distinct stub functions for bytecode and native *)
external launch_ptx_impl
  : CuModulePtr.t -> string -> gpu_arg array ->
         int -> int -> int -> int -> int -> unit =
                "ocaml_pq_launch_ptx_bytecode" "ocaml_pq_launch_ptx"

let inited = ref false

let init () =
  if !inited = false then begin
    inited := true;
    cuda_init ()
  end;
  ()

let launch_ptx (cudaModule : CuModulePtr.t) (fnName : string) 
      (args : gpu_arg array)
      (gridParams : grid_params) =
    IFDEF DEBUG THEN printf "In launch\n%!"; END;
    Timing.start_timer "GPU Execution";
    launch_ptx_impl cudaModule fnName args 
    gridParams.threads_x 
    gridParams.threads_y
        gridParams.threads_z
        gridParams.grid_x
        gridParams.grid_y;
    Timing.stop_timer "GPU Execution"

let cuda_module_from_kernel_list  
      (kernelList : (string * Ptx.kernel) list)
      (threadsPerBlock : int) = 
  let ptxModule = Ptx.module_from_named_kernels kernelList in  
  let ptxStr = Ptx.ptx_module_to_str ptxModule in
  IFDEF DEBUG THEN 
    Printf.printf "%s%!\n" ptxStr;
  ENDIF;
  let modulePtr = compile_module ptxStr threadsPerBlock in
  (* take an input space and change it from referring to 
     kernel-local symids to module-level names 
  *) 
  { 
    module_ptr = modulePtr;
    kernel_names = List.map fst kernelList;  
    threads_per_block = threadsPerBlock; 
  } 
