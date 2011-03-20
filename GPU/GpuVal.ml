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


let elt_to_str gpuVec idx = match gpuVec.vec_t with 
  | DynType.Int32T -> 
    let i32 = Cuda.cuda_get_gpu_int32_vec_elt gpuVec.vec_ptr idx in
    Int32.to_string i32         
  | DynType.Float32T ->
    let f = Cuda.cuda_get_gpu_float32_vec_elt gpuVec.vec_ptr idx in  
    string_of_float f 
  | _ -> "?" 

let elts_summary gpuVec = 
  String.concat ", " $ List.map (elt_to_str gpuVec) (List.til 6) 

let to_str = function 
  | GpuScalar n -> Printf.sprintf "GpuScalar(%s)" (PQNum.num_to_str n)
  | GpuArray gpuVec -> 
      Printf.sprintf "GpuVec(len=%d): [%s]" gpuVec.vec_len (elts_summary gpuVec) 

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
      
let sizeof = function 
  | GpuArray arr -> arr.vec_nbytes 
  | GpuScalar n -> DynType.sizeof (PQNum.type_of_num n)  

  

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


