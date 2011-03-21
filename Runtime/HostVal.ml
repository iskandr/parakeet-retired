(* pp: -parser o pa_macro.cmo *)

open Base
open Printf 

type host_array =
  {
    ptr : Int64.t;
    host_t : DynType.t;
    shape: Shape.t;
    nbytes: int; (* cached to avoid recalculating every time *) 
    (* if this is a slice into some other array, 
       then note the start pointer to avoid calling
       free twice and assist garbage collection 
    *)  
    slice_start: Int64.t option; 
  }

type host_val =
  | HostScalar of PQNum.num
  | HostArray of host_array
  | HostBoxedArray of host_val array

external c_get_int32 : Int64.t -> int -> Int32.t = "ocaml_get_int32"
external c_set_int32 : Int64.t -> int -> Int32.t -> unit = "ocaml_set_int32"

external c_get_float32 : Int64.t -> int -> float = "ocaml_get_float"
external c_set_float32 : Int64.t -> int -> float -> unit = "ocaml_set_float"

external c_get_float64 : Int64.t -> int -> float = "ocaml_get_double"
external c_set_float64 : Int64.t -> int -> float -> unit = "ocaml_set_double"

external c_get_char : Int64.t -> int -> int = "ocaml_get_char"
external c_set_char : Int64.t -> int -> int -> unit = "ocaml_set_char"

let c_get_bool ptr idx = 
  let ch = c_get_char ptr idx in 
  ch > 0  

let c_set_bool ptr idx b = 
  let ch = if b then 1 else 0 in 
  c_set_char ptr idx ch 

let host_vec_to_str hostVec = 
  Printf.sprintf "HostArray { host_t=%s, shape=%s; nbytes=%d; first_word=%ld }"
    (DynType.to_str hostVec.host_t) 
    (Shape.to_str hostVec.shape) 
    hostVec.nbytes
    (c_get_int32 hostVec.ptr 0)

let to_str = function 
  | HostScalar n -> sprintf "HostScalar %s" (PQNum.num_to_str n)
  | HostArray hostVec -> host_vec_to_str hostVec
  | HostBoxedArray _ -> "HostBoxedArray"


let mk_host_scalar n = HostScalar n

let get_type = function 
  | HostArray { host_t = host_t } -> host_t
  | HostScalar n -> PQNum.type_of_num n
  | HostBoxedArray _ -> assert false 

let get_shape = function 
  | HostArray { shape = shape } -> shape
  | HostScalar _ -> Shape.scalar_shape 
  | HostBoxedArray _ -> assert false
   
let get_ptr = function 
  | HostArray { ptr = ptr } -> ptr
  | HostScalar _ -> failwith "Can't get pointer of host scalar"
  | HostBoxedArray _ -> assert false 


let set_vec_elt hostVec idx v =
  IFDEF DEBUG THEN assert (DynType.is_vec hostVec.host_t); ENDIF; 
  match DynType.elt_type hostVec.host_t, v with
  | _, HostArray _ -> failwith "[HostVal] set_vec_elt expected scalar" 
  | DynType.BoolT, HostScalar (PQNum.Bool b) -> 
      c_set_bool hostVec.ptr idx b 
  | DynType.Int32T, HostScalar (PQNum.Int32 i) -> 
      c_set_int32 hostVec.ptr idx i
  | DynType.Float32T, HostScalar (PQNum.Float32 f) -> 
      c_set_float32 hostVec.ptr idx f
  | DynType.Float64T, HostScalar (PQNum.Float64 f) -> 
      c_set_float64 hostVec.ptr idx f
  | _, HostScalar n -> failwith $ 
       Printf.sprintf "[HostVal->set_vec_elt] cannot set elements of %s to %s"
        (DynType.to_str hostVec.host_t)
        (DynType.to_str (PQNum.type_of_num n))
  | _, HostBoxedArray _ -> 
       failwith "[HostVal] boxed array elt set not implemented" 


let get_vec_elt hostVec idx = 
  assert (DynType.is_vec hostVec.host_t);    
  match DynType.elt_type hostVec.host_t with
    | DynType.BoolT -> 
       HostScalar (PQNum.Bool (c_get_bool hostVec.ptr idx)) 
    | DynType.Int32T -> 
        HostScalar (PQNum.Int32 (c_get_int32 hostVec.ptr idx))
    | DynType.Float32T -> 
        HostScalar (PQNum.Float32 (c_get_float32 hostVec.ptr idx))  
    | DynType.Float64T ->  
        HostScalar (PQNum.Float64 (c_get_float64 hostVec.ptr idx))
    | _ -> failwith $ 
       Printf.sprintf "[HostVal->get_vec_elt] cannot get elements of %s"
        (DynType.to_str hostVec.host_t)    

(*
let set_slice array idx elt = match array, elt with 
  | HostArray arr, HostScalar n -> set_vec_elt arr idx elt
  | HostArray arr1, HostArray arr2 -> 
      let sliceShape = Shape.slice_shape arr1.shape [0] in
      let nelts = Shape.nelts sliceShape in 
      let bytesPerElt = DynType.sizeof (DynType.elt_type arr1.host_t) in 
      let sliceSize = nelts * bytesPerElt in
      IFDEF DEBUG THEN
        Printf.printf 
          "[HostVal->set_slice] slice-shape:%s\n" 
          (Shape.to_str sliceShape); 
        Printf.printf "[HostVal->set_slice] nelts:%d\n" nelts;
        Printf.printf "[HostVal->set_slice] bytesPerElt:%d\n" bytesPerElt;
        Printf.printf "[HostVal->set_slice] sliceSize:%d\n" sliceSize; 
        assert (Shape.eq sliceShape arr2.shape); 
      ENDIF;  
      let srcAddress = arr2.ptr in  
      let destAddress = Int64.add arr1.ptr (Int64.of_int (nelts*idx)) in
      for i = 0 to nelts - 1 do 
        (* TODO: fix this, it currently will destroy your memory *)
        set_vec_elt arr1 (i) (get_vec_elt arr2 i)
      done
  | _ -> assert false         
*)
      
let sizeof = function 
  | HostArray arr -> arr.nbytes 
  | HostScalar n -> DynType.sizeof (PQNum.type_of_num n)  
  | HostBoxedArray _ -> assert false 

let slice_vec ({ ptr = ptr; host_t = host_t; shape=shape } as hostArray) idx = 
  let sliceShape = Shape.slice_shape shape [0] in
  let sliceType = DynType.peel_vec host_t in
  if DynType.is_scalar sliceType then 
    get_vec_elt hostArray idx
  else 
    let bytesPerElt = DynType.sizeof (DynType.elt_type sliceType) in 
    let sliceBytes = bytesPerElt * Shape.nelts sliceShape in
    let slicePtr = Int64.add ptr (Int64.of_int (sliceBytes * idx)) in 
    let sliceArray =  { 
      ptr = slicePtr; host_t=sliceType; shape=sliceShape; nbytes=sliceBytes;
      slice_start = Some ptr; 
    }
    in HostArray sliceArray 


(* slice a host array along its outermost dimension, 
   assuming everything is stored in row-major 
 *) 
let slice hostVal idx = match hostVal with 
  | HostScalar _ -> failwith "can't slice a host scalar"
  | HostArray hostVec -> slice_vec hostVec idx 
  | HostBoxedArray _ -> assert false 
