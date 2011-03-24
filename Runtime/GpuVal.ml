(* pp: -parser o pa_macro.cmo *)

open Base
open DynType
open HostVal
open Cuda 

type data_layout = RowMajor | ColumnMajor

let data_layout_to_str = function 
  | RowMajor -> "row-major"
  | ColumnMajor -> "col-major"

type gpu_vec = {
  vec_ptr: Cuda.GpuPtr.t;
  vec_nbytes : int;
  vec_len : int;

  vec_shape_ptr: Cuda.GpuPtr.t;
  vec_shape_nbytes: int;

  vec_shape : Shape.t;
  vec_t : DynType.t;

  (* if this is a slice into some other array,
     then note the start pointer to avoid calling
     free twice and assist garbage collection
  *)
  vec_slice_start: Cuda.GpuPtr.t option;
  
  (* column major version of this same vector *) 
  mutable vec_col_major:  Cuda.GpuPtr.t option;
   
  (*
  vec_data_layout : data_layout;
  *)
}

type gpu_val = GpuScalar of PQNum.num | GpuArray of gpu_vec

let elt_to_str gpuVec idx = 
  match DynType.elt_type gpuVec.vec_t with 
  | DynType.Int32T -> 
    let i32 = Cuda.cuda_get_gpu_int32_vec_elt gpuVec.vec_ptr idx in
    Int32.to_string i32         
  | DynType.Float32T ->
      let f = Cuda.cuda_get_gpu_float32_vec_elt gpuVec.vec_ptr idx in  
    string_of_float f 
  | DynType.CharT 
  | DynType.BoolT ->
    let i =  Cuda.cuda_get_gpu_char_vec_elt gpuVec.vec_ptr idx in
    string_of_int i 
  | _ -> "?" 

let elts_summary gpuVec =
  let maxElts = 12 in 
  let nelts = Shape.nelts gpuVec.vec_shape in 
  let n = min maxElts nelts in 
  let eltStr = 
    String.concat ", " $ List.map (elt_to_str gpuVec) (List.til n)
  in 
  if nelts > maxElts then "[" ^ eltStr ^ " ...]" 
  else "[" ^ eltStr ^ "]"  
   

let gpu_shape_to_str gpuVec =
  let indices = List.til (Shape.rank gpuVec.vec_shape) in 
  let shapeInts = 
    List.map 
      (Cuda.cuda_get_gpu_int32_vec_elt gpuVec.vec_shape_ptr)
      indices
  in 
  "[" ^ (String.concat ", " (List.map Int32.to_string shapeInts)) ^ "]"     
  
let gpu_vec_to_str ?(show_contents=true) gpuVec =
  let basicInfo =  
    Printf.sprintf 
      "GpuVec(%stype=%s, shape=%s, address=%Lx)"
        (if gpuVec.vec_slice_start = None then "" else "SLICE, ") 
        (DynType.to_str gpuVec.vec_t)
        (gpu_shape_to_str gpuVec)
        gpuVec.vec_ptr
  in 
  if show_contents then basicInfo ^ ": " ^ (elts_summary gpuVec)
  else basicInfo 
      

let to_str ?(show_contents=true) = function 
  | GpuScalar n -> Printf.sprintf "GpuScalar(%s)" (PQNum.num_to_str n)
  | GpuArray gpuVec -> gpu_vec_to_str ~show_contents gpuVec
       

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
(*
let get_data_layout = function
  | GpuArray v -> v.vec_data_layout
  | GpuScalar s -> failwith "Can't get data layout for GPU scalar"
*)

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
