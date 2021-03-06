open Base
open HostVal

module CuChanFormatDesc = Int64
module CuModulePtr = Int64
module CuCtxPtr = Int64
module CuTexRef = Int64
module GpuPtr = Int64
module HostPtr = Int64 

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

type cuda_channel_format_kind =
  | Signed
  | Unsigned
  | Float

let infer_channel_format = function 
  | Type.Int32T -> Signed
  (*| Type.UInt32T -> Unsigned*)
  | Type.Float32T -> Float
  | t -> failwith $ 
    Printf.sprintf "Cannot infer texture channel format for type %s"
    (Type.to_str t) 

(** Trying to keep things manageable by keeping these in the same order as they
    are in cuda_stubs.cu **) 
external cuda_init : unit -> unit = "ocaml_cuda_init"
external cuda_init_runtime : unit -> unit = "ocaml_cuda_init_runtime"

external cuda_device_get_count : unit -> int = "ocaml_cuda_device_get_count"
external cuda_device_get_properties : int -> device_info
  = "ocaml_cuda_device_get_properties"
external cuda_device_get_free_and_total_mem : unit -> (int * int)
  = "ocaml_cuda_device_get_free_and_total_mem"

external cuda_ctx_create : int -> CuCtxPtr.t = "ocaml_cuda_ctx_create"
external cuda_ctx_destroy : CuCtxPtr.t -> unit = "ocaml_cuda_ctx_destroy" 

external cuda_malloc' : int -> GpuPtr.t = "ocaml_cuda_malloc"

exception CudaMallocFailed 
let cuda_malloc n =
  if n = 0 then
    failwith "[cuda_malloc] Cannot allocate empty GPU vector"
  else begin
    Timing.start Timing.gpuMalloc;
    let gpuvec = cuda_malloc' n in
    Timing.stop Timing.gpuMalloc;
    if gpuvec = GpuPtr.zero then raise CudaMallocFailed 
    else gpuvec
  end
 
external cuda_free : GpuPtr.t -> unit = "ocaml_cuda_free"

external cuda_memcpy_to_device_impl : HostPtr.t -> GpuPtr.t -> int -> unit
  = "ocaml_cuda_memcpy_to_device"
  
let cuda_memcpy_to_device hostPtr gpuPtr bytes =
  Timing.start Timing.gpuTransfer; 
  cuda_memcpy_to_device_impl hostPtr gpuPtr bytes;
  Timing.stop Timing.gpuTransfer

external cuda_memcpy_to_host_impl : HostPtr.t -> GpuPtr.t -> int -> unit
  = "ocaml_cuda_memcpy_to_host"
  
let cuda_memcpy_to_host hostPtr gpuPtr bytes =
  Timing.start Timing.gpuTransfer;
  cuda_memcpy_to_host_impl hostPtr gpuPtr bytes;
  Timing.stop Timing.gpuTransfer
  
external cuda_memcpy_device_to_device : GpuPtr.t -> GpuPtr.t -> int -> unit 
  = "ocaml_cuda_memcpy_device_to_device"

external 
cuda_memcpy_host_to_symbol : string -> HostPtr.t -> int -> int -> unit
  = "ocaml_cuda_memcpy_host_to_symbol"

external cuda_memcpy_device_to_symbol : 
  CuModulePtr.t -> string -> GpuPtr.t -> int -> int -> unit
  = "ocaml_cuda_memcpy_device_to_symbol"
(*
external 
cuda_memcpy_device_to_symbol : string -> GpuPtr.t -> int -> int -> unit
  = "ocaml_cuda_memcpy_device_to_symbol"  
*)

(** READ ARRAY ELEMENTS **) 

external cuda_get_gpu_int_vec_elt : GpuPtr.t -> int -> int
  = "ocaml_cuda_get_gpu_int_vec_elt"

external cuda_get_gpu_int8_vec_elt : GpuPtr.t -> int -> int 
  = "ocaml_cuda_get_gpu_int8_vec_elt"

external cuda_get_gpu_int16_vec_elt : GpuPtr.t -> int -> int
  = "ocaml_cuda_get_gpu_int16_vec_elt" 
    
external cuda_get_gpu_int32_vec_elt : GpuPtr.t -> int -> Int32.t
  = "ocaml_cuda_get_gpu_int32_vec_elt" 

external cuda_get_gpu_float32_vec_elt : GpuPtr.t -> int -> float 
  = "ocaml_cuda_get_gpu_float32_vec_elt"

(** MODIFY ARRAY ELEMENTS **) 
(*
external cuda_set_gpu_int_vec_element : GpuPtr.t -> int -> int -> unit 
  = "ocaml_cuda_set_gpu_int_vec_element"
*)
external cuda_set_gpu_int32_vec_elt : GpuPtr.t -> int -> Int32.t -> unit
  = "ocaml_cuda_set_gpu_int32_vec_elt" 

external cuda_set_gpu_float32_vec_elt : GpuPtr.t -> int -> float -> unit 
  = "ocaml_cuda_set_gpu_float32_vec_elt"

(** TEXTURES **)

external cuda_module_get_tex_ref : CuModulePtr.t -> string -> CuTexRef.t =
  "ocaml_cuda_module_get_tex_ref"

external cuda_bind_texture_1d_impl
  : CuTexRef.t -> GpuPtr.t -> int -> int -> unit =
  "ocaml_cuda_bind_texture_1d"

let cuda_bind_texture_1d
    (texRef : CuTexRef.t) (devPtr : GpuPtr.t) (length : int) = function
  | Signed -> cuda_bind_texture_1d_impl texRef devPtr length 0
  | Unsigned -> cuda_bind_texture_1d_impl texRef devPtr length 1
  | Float -> cuda_bind_texture_1d_impl texRef devPtr length 2

external cuda_bind_texture_2d_std_channel_impl
  : CuTexRef.t -> GpuPtr.t -> int -> int -> int -> unit =
  "ocaml_cuda_bind_texture_2d_std_channel"

let cuda_bind_texture_2d_std_channel (texRef : CuTexRef.t)
      (devPtr : GpuPtr.t) (width : int) (height : int) = function
  | Signed -> cuda_bind_texture_2d_std_channel_impl texRef devPtr width height 0
  | Unsigned ->
      cuda_bind_texture_2d_std_channel_impl texRef devPtr width height 1
  | Float -> cuda_bind_texture_2d_std_channel_impl texRef devPtr width height 2

let inited = ref false

let init () =
  if !inited = false then begin
    inited := true;
    cuda_init ()
  end;
  ()

(** CUDA MODULE **)
   
(* a module and a list of the kernel names in that module *) 
type cuda_module = {
  module_ptr : CuModulePtr.t;
  kernel_names : string list; 
  threads_per_block : int; (* useless param *) 
}
