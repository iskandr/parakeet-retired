(* pp: -parser o pa_macro.cmo *)

open Base
open Printf 

type host_array =
  {
    ptr : Int64.t;
    host_t : Type.t;
    shape: Shape.t;
    nbytes: int; (* cached to avoid recalculating every time *) 
    (* if this is a slice into some other array, 
       then note the start pointer to avoid calling
       free twice and assist garbage collection 
    *)  
    slice_start: Int64.t option; 
  }

type host_val =
  | HostScalar of ParNum.t
  | HostArray of host_array
  | HostBoxedArray of host_val array


let host_vec_to_str hostVec = 
  Printf.sprintf "HostArray { host_t=%s, shape=%s; nbytes=%d; contents=%ld,%ld,%ld,... }"
    (DynType.to_str hostVec.host_t) 
    (Shape.to_str hostVec.shape) 
    hostVec.nbytes
    (c_get_int32 hostVec.ptr 0)
    (c_get_int32 hostVec.ptr 1)
    (c_get_int32 hostVec.ptr 2)

let to_str = function 
  | HostScalar n -> sprintf "HostScalar %s" (ParNum.num_to_str n)
  | HostArray hostVec -> host_vec_to_str hostVec
  | HostBoxedArray _ -> "HostBoxedArray"


let mk_host_scalar n = HostScalar n

let rec get_type = function 
  | HostArray { host_t = host_t } -> host_t
  | HostScalar n -> ParNum.type_of_num n
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
  IFDEF DEBUG THEN assert (Type.is_vec hostVec.host_t); ENDIF; 
  match Type.elt_type hostVec.host_t, v with
  | _, HostArray _ -> failwith "[HostVal] set_vec_elt expected scalar" 
  | Type.BoolT, HostScalar (ParNum.Bool b) -> 
      c_set_bool hostVec.ptr idx b 
  | Type.Int32T, HostScalar (ParNum.Int32 i) -> 
      c_set_int32 hostVec.ptr idx i
  | Type.Float32T, HostScalar (ParNum.Float32 f) -> 
      c_set_float32 hostVec.ptr idx f
  | Type.Float64T, HostScalar (ParNum.Float64 f) -> 
      c_set_float64 hostVec.ptr idx f
  | _, HostScalar n -> failwith $ 
       Printf.sprintf "[HostVal->set_vec_elt] cannot set elements of %s to %s"
        (Type.to_str hostVec.host_t)
        (Type.to_str (ParNum.type_of_num n))
  | _, HostBoxedArray _ -> 
       failwith "[HostVal] boxed array elt set not implemented" 


let get_vec_elt hostVec idx = 
  assert (Type.is_vec hostVec.host_t);    
  match Type.elt_type hostVec.host_t with
    | Type.BoolT -> 
       HostScalar (ParNum.Bool (c_get_bool hostVec.ptr idx)) 
    | Type.Int32T -> 
        HostScalar (ParNum.Int32 (c_get_int32 hostVec.ptr idx))
    | Type.Float32T -> 
        HostScalar (ParNum.Float32 (c_get_float32 hostVec.ptr idx))  
    | Type.Float64T ->  
        HostScalar (ParNum.Float64 (c_get_float64 hostVec.ptr idx))
    | _ -> failwith $ 
       Printf.sprintf "[HostVal->get_vec_elt] cannot get elements of %s"
        (Type.to_str hostVec.host_t)    

let sizeof = function 
  | HostArray arr -> arr.nbytes 
  | HostScalar n -> Type.sizeof (ParNum.type_of_num n)  
  | HostBoxedArray _ -> assert false 

