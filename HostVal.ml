open Base
open Printf 

type host_array =
  {
    ptr : Int64.t;
    host_t : DynType.t;
    shape: Shape.t;
    nbytes: int; (* cached to avoid recalculating every time *) 
  }
 
type host_val = 
  | HostScalar of PQNum.num 
  | HostArray of host_array 

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

let to_str = function 
  | HostScalar n -> sprintf "HostScalar %s" (PQNum.num_to_str n)
  | HostArray {ptr=ptr; host_t=host_t; shape=shape; nbytes=nbytes} -> 
     sprintf "HostArray { host_t=%s, shape=%s; nbytes=%d; first_word=%ld }"
       (DynType.to_str host_t) 
       (Shape.to_str shape) 
       nbytes
       (c_get_int32 ptr 0)


external c_malloc : int -> Int64.t = "ocaml_malloc"
external c_free : Int64.t -> unit = "ocaml_free"

let free h = if DynType.is_vec h.host_t then c_free h.ptr
       
let mk_host_scalar n = HostScalar n
  
let mk_host_vec ?nbytes ?len ty shape =
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

let get_type = function 
  | HostArray { host_t = host_t } -> host_t
  | HostScalar n -> PQNum.type_of_num n

let get_shape = function 
  | HostArray { shape = shape } -> shape
  | HostScalar _ -> Shape.scalar_shape 

let get_ptr = function 
  | HostArray { ptr = ptr } -> ptr
  | HostScalar _ -> failwith "Can't get pointer of host scalar"

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

(* slice a host array along its outermost dimension, 
   assuming everything is stored in row-major 
 *) 
let get_slice hostVal idx = match hostVal with 
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

let set_slice array idx elt = match array, elt with 
  | HostArray arr, HostScalar n -> set_vec_elt arr idx elt
  | HostArray arr1, HostArray arr2 -> 
      let sliceShape = Shape.slice_shape arr1.shape [0] in
      IFDEF DEBUG THEN assert (Shape.eq sliceShape arr2.shape); ENDIF;  
      let nelts = Shape.nelts sliceShape in 
      let bytesPerElt = DynType.sizeof (DynType.elt_type arr1.host_t) in 
      let sliceSize = nelts * bytesPerElt in
      for i = 0 to nelts - 1 do 
        set_vec_elt arr1 (idx+i) (get_vec_elt arr2 idx)
      done 
      
      
let sizeof = function 
  | HostArray arr -> arr.nbytes 
  | HostScalar n -> DynType.sizeof (PQNum.type_of_num n)  

       
            
      
       
     

