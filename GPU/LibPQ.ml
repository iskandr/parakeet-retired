open Base
open Bigarray
open Cuda
open GpuVal
open Printf

module TimeSpecPtr = Int64

type grid_params = {
  threads_x : int;
  threads_y : int;
  threads_z : int;
  grid_x    : int;
  grid_y    : int;
}

external cuda_init : unit -> unit = "ocaml_pq_init"

(* arguments: a ptx string and a desired number of threads per block *) 
external compile_module
	: string -> int -> CuModulePtr.t = "ocaml_pq_compile_module"

external destroy_module
	: CuModulePtr.t -> unit = "ocaml_pq_destroy_module"

(* bytecode handles arity > 5 differently from natively compiled OCaml, *)
(* so need two distinct stub functions for bytecode and native *)
external launch_ptx_impl
  : CuModulePtr.t -> string -> GpuVal.gpu_val array ->
		 int -> int -> int -> int -> int -> unit =
				"ocaml_pq_launch_ptx_bytecode" "ocaml_pq_launch_ptx"
	
let launch_ptx (cudaModule : CuModulePtr.t) (fnName : string) 
      (args : GpuVal.gpu_val array)
      (gridParams : grid_params) =
  printf "In launch\n%!";
	launch_ptx_impl cudaModule fnName args 
    gridParams.threads_x 
    gridParams.threads_y
		gridParams.threads_z
		gridParams.grid_x
		gridParams.grid_y

(* external get_time : unit -> TimeSpecPtr.t = "ocaml_pq_gettime" 
external time_mem_transfer : int -> float = "ocaml_pq_time_mem_transfer"

external diff_timers : TimeSpecPtr.t -> TimeSpecPtr.t -> float =
  "ocaml_pq_diff_timers"

*)

