open Base
open HostVal

module HostPtr = Int64 
module GpuPtr = Int32
module CuCtxPtr = Int64
module CuChanFormatDesc = Int64
module CuModulePtr = Int64
module CuTexRef = Int32

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
type cuda_module = {
  module_ptr : CuModulePtr.t;
  kernels : (string * (string Ptx.calling_conventions)) list;
  threads_per_block : int 
}

type cuda_channel_format_kind =
  | Signed
  | Unsigned
  | Float

let infer_channel_format = function 
  | DynType.Int32T -> Signed
  | DynType.UInt32T -> Unsigned
  | DynType.Float32T -> Float
  | t -> failwith $ 
    Printf.sprintf "Cannot infer texture channel format for type %s"
    (DynType.to_str t) 

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

external cuda_module_get_tex_ref : CuModulePtr.t -> string -> CuTexRef.t =
  "ocaml_cuda_module_get_tex_ref"

external cuda_bind_texture_2d_std_channel_impl
  : CuTexRef.t -> GpuPtr.t -> int -> int -> int -> unit =
  "ocaml_cuda_bind_texture_2d_std_channel"

let cuda_bind_texture_2d_std_channel (texRef : CuTexRef.t)
      (devPtr : GpuPtr.t) (width : int) (height : int) = function
  | Signed -> cuda_bind_texture_2d_std_channel_impl texRef devPtr width height 0
  | Unsigned ->
      cuda_bind_texture_2d_std_channel_impl texRef devPtr width height 1
  | Float -> cuda_bind_texture_2d_std_channel_impl texRef devPtr width height 2
  | _ -> failwith "[cuda] Unsupported texture type"
