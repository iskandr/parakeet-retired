(* pp: -parser o pa_macro.cmo *)

open Base 
open GpuVal 
open HostVal 

external c_malloc : int -> Int64.t = "ocaml_malloc"
external c_free : Int64.t -> unit = "ocaml_free"
external c_memcpy : Int64.t -> Int64.t -> int -> unit = "ocaml_memcpy"    
    
external get_array1_ptr 
  : ('a,'b,'c) Bigarray.Array1.t -> Int64.t = "get_bigarray_ptr"

external get_array2_ptr 
  : ('a,'b,'c) Bigarray.Array2.t -> Int64.t = "get_bigarray_ptr"

external get_array3_ptr 
  : ('a,'b,'c) Bigarray.Array3.t -> Int64.t = "get_bigarray_ptr"



(* send a shape vector the gpu *) 
let shape_to_gpu shape =
  let shapeBytes = Shape.nbytes shape in
  IFDEF DEBUG THEN 
    Printf.printf "Sending shape to GPU: %s (%d bytes)\n"
      (Shape.to_str shape)
      shapeBytes
  ENDIF;
  let shapeDevPtr = Cuda.cuda_malloc shapeBytes in
  let shapeHostPtr = 
    get_array1_ptr $ Shape.to_c_array shape 
  in
  Cuda.cuda_memcpy_to_device shapeHostPtr shapeDevPtr shapeBytes;
  shapeDevPtr, shapeBytes


(* creates a fresh vec on the gpu -- allow optional size argument if that's 
   already precomputed *)
let alloc_gpu_vec ?nbytes ?len ty shape =
  let len = match len with None -> Shape.nelts shape | Some len -> len in
  let nbytes = match nbytes with
    | None ->
       let eltT = DynType.elt_type ty in
       let eltSize = DynType.sizeof eltT in
       len * eltSize 
    | Some n -> n 
  in
  IFDEF DEBUG THEN Printf.printf "Making GPU vec of %d bytes\n" nbytes; ENDIF;
  let outputPtr = Cuda.cuda_malloc nbytes in
  let shapePtr, shapeSize = shape_to_gpu shape in
  let gpuVec = {
    vec_ptr = outputPtr;
    vec_nbytes = nbytes;
    vec_len = len;

    vec_shape_ptr = shapePtr;
    vec_shape_nbytes = shapeSize;

    vec_shape = shape;
    vec_t = ty;
    vec_slice_start = None; 

  } in 
  IFDEF DEBUG THEN
    Printf.printf "[Alloc] Created %s\n" (GpuVal.to_str (GpuArray gpuVec));
    Pervasives.flush_all(); 
  ENDIF;  
  gpuVec 
  
let alloc_host_vec ?nbytes ?len ty shape =
  let len = match len with None -> Shape.nelts shape | Some len -> len in 
  let nbytes = match nbytes with 
    | None ->
       let eltT = DynType.elt_type ty in 
       let eltSize = DynType.sizeof eltT in
       len * eltSize 
    | Some n -> n 
  in  
  let ptr = c_malloc nbytes in
  {
    ptr = ptr; 
    nbytes = nbytes; 
    host_t = ty; 
    shape = shape;
    slice_start = None  
  }

let delete_gpu_vec gpuVec =
  (* don't free a gpu vector if it's a slice into some other value *) 
  if gpuVec.vec_slice_start = None && gpuVec.vec_nbytes > 0 then (
    Cuda.cuda_free gpuVec.vec_ptr;
    if gpuVec.vec_shape_nbytes > 0 then  Cuda.cuda_free gpuVec.vec_shape_ptr;
  )

let delete_gpu_val = function 
  | GpuScalar _ -> ()
  | GpuArray gpuVec -> 
    IFDEF DEBUG THEN
      Printf.printf "[Alloc] Deleting %s\n" (GpuVal.to_str (GpuArray gpuVec));
      Pervasives.flush_all(); 
    ENDIF;  
    delete_gpu_vec gpuVec 

let delete_host_vec h = 
    if DynType.is_vec h.host_t && h.slice_start = None then c_free h.ptr

let rec delete_host_val = function 
  | HostVal.HostScalar _ -> ()
  | HostVal.HostBoxedArray boxedArray -> 
      Array.iter delete_host_val boxedArray 
  | HostVal.HostArray unboxed -> delete_host_vec unboxed
  

  
let to_gpu  = function 
  | HostScalar n -> GpuScalar n
  | HostArray { ptr=ptr; host_t=host_t; shape=shape; nbytes=nbytes } ->
    let gpuPtr = Cuda.cuda_malloc nbytes in 
    Cuda.cuda_memcpy_to_device ptr gpuPtr nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu shape in
    let gpuVec =  {
      vec_ptr = gpuPtr; 
      vec_nbytes = nbytes;
      vec_len= Shape.nelts shape; 

      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 

      vec_shape = shape;
      vec_t = host_t; 
      vec_slice_start=None; 
    }
    in 
    IFDEF DEBUG THEN 
      Printf.printf "[Alloc] vector sent to GPU: %s\n"
        (GpuVal.gpu_vec_to_str gpuVec)
      ; 
    ENDIF; 
    GpuVal.GpuArray gpuVec 
    
let from_gpu ?prealloc = function 
    | GpuScalar n -> HostScalar n
    | GpuArray v ->
        let dataHostPtr =  match prealloc with 
          | None -> c_malloc v.vec_nbytes 
          | Some ptr -> ptr 
        in 
        Cuda.cuda_memcpy_to_host dataHostPtr v.vec_ptr v.vec_nbytes; 
        HostArray { 
          ptr = dataHostPtr; 
          nbytes = v.vec_nbytes;
          host_t = v.vec_t; 
          shape = v.vec_shape; 
          slice_start = None; 
        }