(* pp: -parser o pa_macro.cmo *)

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

let gpu_args_to_str args = 
  String.concat ", " (Array.to_list (Array.map gpu_arg_to_str args))

(* arguments: a ptx string and a desired number of threads per block *) 
external compile_module_impl
    : string -> int -> CuModulePtr.t = "ocaml_cuda_compile_module"
let compile_module ptx threadsPerBlock =
  Timing.start Timing.ptxCompile; 
  let modulePtr = compile_module_impl ptx threadsPerBlock in
  Timing.stop Timing.ptxCompile; 
  modulePtr

external destroy_module
    : CuModulePtr.t -> unit = "ocaml_cuda_destroy_module"

(* bytecode handles arity > 5 differently from natively compiled OCaml, *)
(* so need two distinct stub functions for bytecode and native *)
external launch_ptx_impl : 
    CuModulePtr.t -> string -> gpu_arg array -> int ->
                     int -> int -> int -> int -> unit =
  "ocaml_cuda_launch_ptx_bytecode" "ocaml_cuda_launch_ptx"

let launch_ptx (cudaModule : CuModulePtr.t) (fnName : string)
      (args : gpu_arg array)
      (gridParams : grid_params) =
    Timing.start Timing.gpuExec;
    launch_ptx_impl
      cudaModule fnName args
      gridParams.threads_x
      gridParams.threads_y
      gridParams.threads_z
      gridParams.grid_x
      gridParams.grid_y;
    Timing.stop Timing.gpuExec

let cuda_module_from_kernel_list  
      (kernelList : (string * Ptx.kernel) list)
      (threadsPerBlock : int) = 
  let ptxModule = Ptx.module_from_named_kernels kernelList in  
  let ptxStr = Ptx.ptx_module_to_str ptxModule in
  let modulePtr = compile_module ptxStr threadsPerBlock in
  (* take an input space and change it from referring to 
     kernel-local symids to module-level names 
  *) 
  { 
    module_ptr = modulePtr;
    kernel_names = List.map fst kernelList;  
    threads_per_block = threadsPerBlock; 
  } 
