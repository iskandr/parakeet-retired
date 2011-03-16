(* pp: -parser o pa_macro.cmo *)

open Base
open DynType
open HostVal
open Cuda 

type gpu_vec = {
  vec_ptr: Int64.t;
  vec_nbytes : int;
  vec_len : int;

  vec_shape_ptr: Int64.t;
  vec_shape_nbytes: int;

  vec_shape : Shape.t;
  vec_t : DynType.t;
  
  (* if this is a slice into some other array, 
     then note the start pointer to avoid calling
     free twice and assist garbage collection 
  *)  
  vec_slice_start: Int64.t option; 
}

type gpu_val = GpuScalar of PQNum.num | GpuArray of gpu_vec

let to_str = function 
  | GpuScalar n -> Printf.sprintf "GpuScalar(%s)" (PQNum.num_to_str n)
  | GpuArray {vec_len=vec_len} -> Printf.sprintf "GpuVec[%d]" vec_len

let nelts = function
  | GpuScalar _-> 1
  | GpuArray v -> Shape.nelts v.vec_shape

let get_shape = function
  | GpuArray v -> v.vec_shape
  | GpuScalar _ -> Shape.scalar_shape

let get_shape_ptr = function
  | GpuArray v -> v.vec_shape_ptr
  | GpuScalar _ -> failwith "Can't get shape pointer for "

let get_shape_nbytes = function
  | GpuArray v -> v.vec_shape_nbytes
  | GpuScalar _ -> failwith "Don't know how to get num bytes for scalar shape"

let get_type = function
  | GpuArray v -> v.vec_t
  | GpuScalar n -> (PQNum.type_of_num n)

let get_ptr = function
  | GpuArray v -> v.vec_ptr
  | GpuScalar _ -> failwith "Can't get GPU pointer to a scalar"

let get_scalar = function
  | GpuArray _ -> failwith "Can't get scalar for GPU vector"
  | GpuScalar s -> s

let get_nbytes = function
  | GpuArray v -> v.vec_nbytes
  | GpuScalar s -> DynType.sizeof (PQNum.type_of_num s)

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
  IFDEF DEBUG THEN 
    Printf.printf "Sending shape to GPU: %s (%d bytes)\n"
      (Shape.to_str shape)
      shapeBytes
  ENDIF;
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
  IFDEF DEBUG THEN Printf.printf "Making GPU vec of %d bytes\n" nbytes; ENDIF;
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
      | None -> (
        IFDEF DEBUG THEN
          Printf.printf "to_gpu for %d bytes\n" nbytes;
          flush stdout;
        ENDIF;
        cuda_malloc nbytes)
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

let get_slice gpuVal idx = 
  IFDEF DEBUG THEN  
    Printf.printf "[GpuVal->slice] %s @ %d\n%!" (to_str gpuVal) idx
  ENDIF; 
  match gpuVal with   
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
          vec_ptr = Int64.add arr.vec_ptr (Int64.of_int (sliceBytes * idx));  
          vec_nbytes = sliceBytes; 
          vec_len = nelts; 
          vec_shape_ptr = Int64.add arr.vec_shape_ptr (Int64.of_int 4); 
          vec_shape_nbytes = arr.vec_shape_nbytes - 4; 
          vec_shape = sliceShape; 
          vec_t = sliceType;
          vec_slice_start = Some arr.vec_ptr; 
      }
      in 
      GpuArray sliceInfo  
