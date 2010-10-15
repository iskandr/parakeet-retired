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

type gpu_scalar = {
  scalar_data : Int64.t;
  scalar_t : DynType.t;
  scalar_nbytes : int;
}

type gpu_val = GpuVec of gpu_vec | GpuScalar of gpu_scalar

let free = function
  | GpuScalar _ -> ()
  | GpuVec v ->  cuda_free v.vec_ptr; cuda_free v.vec_shape_ptr

let nelts = function
  | GpuScalar _-> 1
  | GpuVec v -> Shape.nelts v.vec_shape

let get_shape = function
  | GpuVec v -> v.vec_shape
  | GpuScalar _ -> Shape.empty

let get_type = function
  | GpuVec v -> v.vec_t
  | GpuScalar s -> s.scalar_t

let mk_scalar n =
  let scalar = match n with  
  | PQNum.Int i -> 
    {scalar_data=HostPtr.of_int i; scalar_t=Int64T; scalar_nbytes=8}
  | PQNum.Int32 i -> 
    {scalar_data=HostPtr.of_int32 i; scalar_t=Int32T; scalar_nbytes=4}  
  | PQNum.Int64 i -> 
    { scalar_data=HostPtr.of_int64 i; scalar_t=Int64T; scalar_nbytes=8}  
  | PQNum.Float32 f ->
    { scalar_data=Int64.of_int32 (Float32.bits32_of_float f); 
      scalar_t=Float32T; 
      scalar_nbytes=4
    }
  | PQNum.Float64 f ->
    { scalar_data=HostPtr.bits_of_float f; scalar_t=Float64T; scalar_nbytes=8} 
  | PQNum.Bool b -> 
    { scalar_data = HostPtr.of_bool b; scalar_t = BoolT; scalar_nbytes=2}
  | PQNum.Char c -> 
    { scalar_data = HostPtr.of_char c; scalar_t = CharT; scalar_nbytes=1}
  in GpuScalar scalar

(* send a shape vector the gpu *) 
let shape_to_gpu shape =
  let shapeBytes = Shape.nbytes shape in
  let shapeDevPtr = cuda_malloc shapeBytes in
  let shapeHostPtr = Shape.get_raw_ptr shape in
  cuda_memcpy_to_device shapeHostPtr shapeDevPtr shapeBytes;
  shapeDevPtr, shapeBytes

(* creates a fresh vec on the gpu *)
let mk_gpu_vec ty shape size =
  let outputPtr = cuda_malloc size in
  let shapePtr, shapeSize = shape_to_gpu shape in
  GpuVec {
    vec_ptr = outputPtr;
    vec_nbytes = size;
    vec_len = Shape.nelts shape;

    vec_shape_ptr = shapePtr;
    vec_shape_nbytes = shapeSize;

    vec_shape = shape;
    vec_t = ty;
  }

let to_gpu hostVal =
  if is_scalar hostVal.host_t then 
    GpuScalar {
     scalar_nbytes =  hostVal.nbytes; 
     scalar_t = hostVal.host_t; 
     scalar_data = hostVal.ptr
    } 
  else
    (let gpuPtr = cuda_malloc hostVal.nbytes in
    cuda_memcpy_to_device hostVal.ptr gpuPtr hostVal.nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu hostVal.shape in
    GpuVec  {
      vec_ptr = gpuPtr; 
      vec_nbytes = hostVal.nbytes;
      vec_len= Shape.nelts hostVal.shape; 

      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 

      vec_shape = hostVal.shape;  
      vec_t = hostVal.host_t 
    }
    )
let from_gpu = function 
    | GpuScalar s ->
        {
          ptr = s.scalar_data;
          host_t = s.scalar_t;
          nbytes = s.scalar_nbytes;
          shape = Shape.empty;
        }

    | GpuVec v ->
        let dataHostPtr = c_malloc v.vec_nbytes in
        cuda_memcpy_to_host dataHostPtr v.vec_ptr v.vec_nbytes; 
        { 
          ptr = dataHostPtr; 
          nbytes = v.vec_nbytes;
          host_t = v.vec_t; 
          shape = v.vec_shape; 
        }
