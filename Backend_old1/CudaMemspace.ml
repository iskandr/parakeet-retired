

external cuda_free : Int64.t -> unit = "ocaml_cuda_free"

(* host ptr -> gpu ptr -> nbytes -> unit *) 
external cuda_memcpy_to_device_impl : Int64.t  -> Int64.t -> int -> unit
  = "ocaml_cuda_memcpy_to_device"
  
(* host ptr -> gpu ptr -> nbytes -> unit *) 
external cuda_memcpy_to_host_impl : Int64.t -> Int64.t  -> int -> unit
  = "ocaml_cuda_memcpy_to_host"

class ptr addr = object

  method free = cuda_free addr 
   
  method memcpy_from_host (hostAddr:Int64.t) (nbytes:int) = 
    Timing.start Timing.gpuTransfer; 
    cuda_memcpy_to_device_impl hostAddr addr nbytes;
    Timing.stop Timing.gpuTransfer
  
  method memcpy_to_host (hostAddr:Int64.t) (nbytes:int) = 
    Timing.start Timing.gpuTransfer;
    cuda_memcpy_to_host_impl hostPtr addr bytes;
    Timing.stop Timing.gpuTransfer
end

let of_int64 addr = new ptr addr

external cuda_malloc' : int -> GpuPtr.t = "ocaml_cuda_malloc"
exception CudaMallocFailed 

let alloc n =
  if n = 0 then failwith "[cuda_malloc] Cannot allocate empty GPU vector"
  else begin
    Timing.start Timing.gpuMalloc;
    let addr = cuda_malloc' n in
    Timing.stop Timing.gpuMalloc;
    if addr = Int64.zero then raise CudaMallocFailed 
    else of_int64 addr  
      
  end

 