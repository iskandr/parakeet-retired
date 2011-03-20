(* pp: -parser o pa_macro.cmo *)

open GpuVal 
open HostVal 


external c_malloc : int -> Int64.t = "ocaml_malloc"
external c_free : Int64.t -> unit = "ocaml_free"
external c_memcpy : Int64.t -> Int64.t -> int -> unit = "ocaml_memcpy"

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
  }


let delete_gpu_val memState gpuVal = function
  | GpuScalar _ -> ()
  | GpuArray v ->
      (* don't free a gpu vector if it's a slice into some other 
         value 
      *) 
      if v.vec_slice_start = None then   
        (cuda_free v.vec_ptr; cuda_free v.vec_shape_ptr)



let delete_host_vec h = if DynType.is_vec h.host_t then c_free h.ptr


                
let gpu_slice gpuVal idx = 
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

(* slice a host array along its outermost dimension, 
   assuming everything is stored in row-major 
 *) 
let host_slice hostVal idx = match hostVal with 
  | HostScalar _ -> failwith "can't slice a host scalar"
  | HostArray ({ ptr = ptr; host_t = host_t; shape=shape } as hostArray) -> 
    let sliceShape = Shape.slice_shape shape [0] in
    let sliceType = DynType.peel_vec host_t in
    if DynType.is_scalar sliceType then 
      get_vec_elt hostArray idx
    else 
      let bytesPerElt = DynType.sizeof (DynType.elt_type sliceType) in 
      let sliceBytes = bytesPerElt * Shape.nelts sliceShape in
      let slicePtr = Int64.add ptr (Int64.of_int (sliceBytes * idx)) in 
      let sliceArray =  { 
        ptr = slicePtr; host_t=sliceType; shape=sliceShape; nbytes=sliceBytes 
      }
      in HostArray sliceArray 
  | HostBoxedArray _ -> assert false 
  