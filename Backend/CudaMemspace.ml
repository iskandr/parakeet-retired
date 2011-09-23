
external cuda_free : Int64.t -> unit = "ocaml_cuda_free"

(* host ptr -> gpu ptr -> nbytes -> unit *) 
external cuda_memcpy_to_device_impl : Int64.t  -> Int64.t -> int -> unit
  = "ocaml_cuda_memcpy_to_device"
  
(* host ptr -> gpu ptr -> nbytes -> unit *) 
external cuda_memcpy_to_host_impl : Int64.t -> Int64.t  -> int -> unit
  = "ocaml_cuda_memcpy_to_host"

(* HOW TO MAKE THIS WORK WITH MULTIPLE GPUs? *) 
let memcpy_from_host ~(src:Int64.t) ~(dest:In64.t) ~(nbytes:int) =  
      Timing.start Timing.gpuTransfer; 
      cuda_memcpy_to_device_impl src dest nbytes; 
      Timing.stop Timing.gpuTransfer
  
let memcpy_to_host ~(src:Int64.t) ~(dest:In64.t) ~(nbytes:int) = 
      Timing.start Timing.gpuTransfer;
      cuda_memcpy_to_host_impl dest src bytes;
      Timing.stop Timing.gpuTransfer


external cuda_malloc' : int -> GpuPtr.t = "ocaml_cuda_malloc"
exception CudaMallocFailed 

let raw_alloc n : Int64.t =
  if n = 0 then failwith "[cuda_malloc] Cannot allocate empty GPU vector"
  else begin
    Timing.start Timing.gpuMalloc;
    let addr = cuda_malloc' n in
    Timing.stop Timing.gpuMalloc;
    if addr = Int64.zero then raise CudaMallocFailed 
    else addr  
  end


class ptr addr = object

  method free = cuda_free addr 
   
end

let of_int64 addr = new ptr addr
let alloc nbytes = of_int64 (raw_alloc nbytes)
