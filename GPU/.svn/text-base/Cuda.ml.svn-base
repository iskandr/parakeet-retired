open HostVal

module GpuPtr = Int32
module CuCtxPtr = Int64
module CuModulePtr = Int64

type device_info = {
  max_threads_per_block : int;
  max_threads_per_block_x : int;
  max_threads_per_block_y : int;
  max_threads_per_block_z : int;
  max_blocks_per_grid_x : int;
  max_blocks_per_grid_y : int;
  max_blocks_per_grid_z : int;
  shared_mem_per_block : int;
  total_constant_mem : int;
  warp_size : int;
  max_mem_pitch : int;
  regs_per_block : int;
  clock_rate_khz : int;
  texture_align : int;
}

(* a module and a list of the kernel names in that module *) 
type compiled_kernel = {
  module_ptr : CuModulePtr.t;
  kernel_names : string list;
  threads_per_block : int 
} 

external cuda_malloc : int -> GpuPtr.t = "ocaml_cuda_malloc"
external cuda_free : GpuPtr.t -> unit = "ocaml_cuda_free"

external cuda_device_get_count : unit -> int = "ocaml_cuda_device_get_count"
external cuda_device_get_properties : int -> device_info
  = "ocaml_cuda_device_get_properties"

external cuda_ctx_create : int -> CuCtxPtr.t = "ocaml_cuda_ctx_create"
external cuda_ctx_destroy : CuCtxPtr.t -> unit = "ocaml_cuda_ctx_destroy"

external cuda_memcpy_to_device  : HostPtr.t -> GpuPtr.t -> int -> unit
  = "ocaml_cuda_memcpy_to_device"

external cuda_memcpy_to_host : HostPtr.t -> GpuPtr.t -> int -> unit
  = "ocaml_cuda_memcpy_to_host"

external cuda_device_get_count : unit -> int = "ocaml_cuda_device_get_count"
