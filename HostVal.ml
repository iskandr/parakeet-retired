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


let to_str = function 
  | HostScalar n -> sprintf "HostScalar %s" (PQNum.num_to_str n)
  | HostArray {ptr=ptr; host_t=host_t; shape=shape; nbytes=nbytes} -> 
     sprintf "HostArray { host_t=%s, shape=%s; nbytes=%d }"
       (DynType.to_str host_t) 
       (Shape.to_str shape) 
       nbytes

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

(*
let set_vec_elt hostVec idx v =
  assert (DynType.is_vec hostVec.host_t);
  match DynType.elt_type hostVec.host_t, v with
  | _, HostArray _ -> failwith "expected scalar" 
  | DynType.Int32T, HostScalar (PQNum.Int32 i) -> c_set_int32 hostVec.ptr idx i
  | DynType.Float64T, HostScalar (PQNum.Float64 f) ->
     c_set_float hostVec.ptr idx f
  | _, HostScalar n -> failwith $ 
       Printf.sprintf "[HostVal->set_vec_elt] cannot set elements of %s to %s"
        (DynType.to_str hostVec.host_t)
        (PQNum.type_of_num n)
*) 
(*
let get_vec_elt hostVec idx = 
  assert (DynType.is_vec hostVec.host_t);    
  match DynType.elt_type hostVec.host_t with 
    | DynType.Int32T -> 
        HostScalar (PQNum.Int32 (c_get_int32 hostVec.ptr idx))  
    | DynType.Float64T ->  
        HostScalar (PQNum.Float64 (c_get_float hostVec.ptr idx))
    | _ -> failwith 
       (Printf.sprintf "[HostVal->get_vec_elt] cannot get elements of %s"
        (DynType.to_str hostVec.host_t)
        
       )
*)    