external cuda_free : Int64.t -> unit = "ocaml_cuda_free"

(* host ptr -> gpu ptr -> nbytes -> unit *) 
external cuda_memcpy_to_device_impl : Int64.t  -> Int64.t -> int -> unit
  = "ocaml_cuda_memcpy_to_device"
  
(* host ptr -> gpu ptr -> nbytes -> unit *) 
external cuda_memcpy_to_host_impl : Int64.t -> Int64.t  -> int -> unit
  = "ocaml_cuda_memcpy_to_host"
 
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

let memspace_id = MemspaceRegistry.register "gpu"


class ptr addr = object
  method free = cuda_free addr 
  method addr = addr  
 
  method memspace_id =  memspace_id
  
  method get_bool : int -> bool
  method get_char : int -> char
  method get_int32 : int -> Int32.t 
  method get_int64 : int -> Int64.t 
  method get_float32 : int -> float
  method get_float64 : int -> float
end 
  
end

(* create pointer object and cast to pointer base type *) 
let of_int64 addr = (new ptr addr) :> Ptr.t 
let alloc nbytes = of_int64 (raw_alloc nbytes)
