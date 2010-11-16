open Base
open DynType
open HostVal
open Cuda 

type gpu_vec = {
  vec_ptr: Int32.t;
  vec_nbytes : int;
  vec_len : int;

  vec_shape_ptr: Int32.t;
  vec_shape_nbytes: int;

  vec_shape : Shape.t;
  vec_t : DynType.t;
  
  (* if this is a slice into some other array, 
     then note the start pointer to avoid calling
     free twice and assist garbage collection 
  *)  
  vec_slice_start: Int32.t option; 
}

type gpu_val = 
  | GpuScalar of PQNum.num 
  | GpuArray of gpu_vec 


let nelts = function
  | GpuScalar _-> 1
  | GpuArray v -> Shape.nelts v.vec_shape

let get_shape = function
  | GpuArray v -> v.vec_shape
  | GpuScalar _ -> Shape.scalar_shape 

let get_type = function
  | GpuArray v -> v.vec_t
  | GpuScalar n -> (PQNum.type_of_num n)

let get_ptr = function 
  | GpuArray v -> v.vec_ptr 
  | GpuScalar _ -> failwith "Can't get GPU pointer to a scalar"

let mk_scalar n = GpuScalar n  


let free = function
  | GpuScalar _ -> ()
  | GpuArray v ->
      (* don't free a gpu vector if it's a slice into some other 
         value 
      *) 
      if v.vec_slice_start = None then   
        (cuda_free v.vec_ptr; cuda_free v.vec_shape_ptr)
      
    
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
    vec_slice_start = None; 
  }

let sizeof = function 
  | GpuArray arr -> arr.vec_nbytes 
  | GpuScalar n -> DynType.sizeof (PQNum.type_of_num n)  

let to_gpu ?prealloc = function 
  | HostScalar n -> GpuScalar n
  | HostArray { ptr=ptr; host_t=host_t; shape=shape; nbytes=nbytes } ->
    let gpuPtr = match prealloc with 
      | None -> cuda_malloc nbytes 
      | Some ptr -> ptr 
    in 
    cuda_memcpy_to_device ptr gpuPtr nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu shape in
    GpuArray  {
      vec_ptr = gpuPtr; 
      vec_nbytes = nbytes;
      vec_len= Shape.nelts shape; 

      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 

      vec_shape = shape;
      vec_t = host_t; 
      vec_slice_start=None; 
    }
    
let from_gpu ?prealloc = function 
    | GpuScalar n -> HostScalar n
    | GpuArray v ->
        let dataHostPtr =  match prealloc with 
          | None -> c_malloc v.vec_nbytes 
          | Some ptr -> ptr 
        in 
        cuda_memcpy_to_host dataHostPtr v.vec_ptr v.vec_nbytes; 
        HostArray { 
          ptr = dataHostPtr; 
          nbytes = v.vec_nbytes;
          host_t = v.vec_t; 
          shape = v.vec_shape; 
        }

  

let index arr idx =
  match DynType.elt_type arr.vec_t with 
    | DynType.Int32T -> 
        let i32 = Cuda.cuda_get_gpu_int32_vec_elt arr.vec_ptr idx in 
        GpuScalar (PQNum.Int32 i32)         
    | DynType.Float32T ->
        let f = Cuda.cuda_get_gpu_float32_vec_elt arr.vec_ptr idx in  
        GpuScalar (PQNum.Float32 f) 

    | _ -> failwith $ Printf.sprintf 
               "Indexing into GPU vectors not implemented for type %s"
               (DynType.to_str $  DynType.elt_type arr.vec_t)

let get_slice gpuVal idx = match gpuVal with   
  | GpuScalar _ -> failwith "can't slice a GPU scalar"
  | GpuArray arr ->
    let sliceShape = Shape.slice_shape arr.vec_shape [0] in
    let sliceType = DynType.peel_vec arr.vec_t in
    if DynType.is_scalar sliceType then 
      index arr idx
    else 
      let bytesPerElt = DynType.sizeof (DynType.elt_type sliceType) in
      let nelts =  Shape.nelts sliceShape in 
      let sliceBytes = bytesPerElt * nelts in
      let sliceInfo = { 
          vec_ptr = Int32.add arr.vec_ptr (Int32.of_int (sliceBytes * idx));  
          vec_nbytes = sliceBytes; 
          vec_len = nelts; 
          vec_shape_ptr = Int32.add arr.vec_shape_ptr (Int32.of_int 4); 
          vec_shape_nbytes = arr.vec_shape_nbytes - 4; 
          vec_shape = sliceShape; 
          vec_t = sliceType;
          vec_slice_start = Some arr.vec_ptr; 
      }
      in 
      GpuArray sliceInfo  

