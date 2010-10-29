open Base
open Cuda
open DynType
open HostVal

type gpu_vec = {
  vec_ptr: GpuPtr.t;
  vec_nbytes : int;
  vec_len : int;

  vec_shape_ptr: GpuPtr.t;
  vec_shape_nbytes: int;

  vec_shape : Shape.t;
  vec_t : DynType.t;
}

type gpu_val = GpuScalar of PQNum.num | GpuArray of gpu_vec  

let free = function
  | GpuScalar _ -> ()
  | GpuArray v ->  cuda_free v.vec_ptr; cuda_free v.vec_shape_ptr

let nelts = function
  | GpuScalar _-> 1
  | GpuArray v -> Shape.nelts v.vec_shape

let get_shape = function
  | GpuArray v -> v.vec_shape
  | GpuScalar _ -> Shape.scalar_shape 

let get_type = function
  | GpuArray v -> v.vec_t
  | GpuScalar n -> (PQNum.type_of_num n)

let mk_scalar n = GpuScalar n  
  
(* send a shape vector the gpu *) 
let shape_to_gpu shape =
  let shapeBytes = Shape.nbytes shape in
  let shapeDevPtr = cuda_malloc shapeBytes in
  let shapeHostPtr = get_array1_ptr $ Shape.to_c_array shape in
  cuda_memcpy_to_device shapeHostPtr shapeDevPtr shapeBytes;
  shapeDevPtr, shapeBytes

(* creates a fresh vec on the gpu -- allow optional size argument if that's 
   already precomputed *)
let mk_gpu_vec ?nbytes ?len ty shape =
  let len = match len with None -> Shape.nelts shape | Some len -> len in 
  let nbytes = match nbytes with 
    | None ->
       let eltT = DynType.elt_type ty in 
       let eltSize = DynType.sizeof eltT in
       len * eltSize 
    | Some n -> n 
  in  
  let outputPtr = cuda_malloc nbytes in

  let shapePtr, shapeSize = shape_to_gpu shape in
  GpuArray {
    vec_ptr = outputPtr;
    vec_nbytes = nbytes;
    vec_len = len;

    vec_shape_ptr = shapePtr;
    vec_shape_nbytes = shapeSize;

    vec_shape = shape;
    vec_t = ty;
  }

let to_gpu = function 
  | HostScalar n -> GpuScalar n
  | HostArray { ptr=ptr; host_t=host_t; shape=shape; nbytes=nbytes } ->  
    let gpuPtr = cuda_malloc nbytes in
    cuda_memcpy_to_device ptr gpuPtr nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu shape in
    GpuArray  {
      vec_ptr = gpuPtr; 
      vec_nbytes = nbytes;
      vec_len= Shape.nelts shape; 

      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 

      vec_shape = shape;  
      vec_t = host_t 
    }
    
let from_gpu = function 
    | GpuScalar n -> HostScalar n
    | GpuArray v ->
        let dataHostPtr = c_malloc v.vec_nbytes in
        cuda_memcpy_to_host dataHostPtr v.vec_ptr v.vec_nbytes; 
        HostArray { 
          ptr = dataHostPtr; 
          nbytes = v.vec_nbytes;
          host_t = v.vec_t; 
          shape = v.vec_shape; 
        }
