open Base

type host_val = {
  ptr : HostPtr.t;
  nbytes: int;
  host_t : DynType.t;
  shape: Shape.t;
}

external c_malloc : int -> HostPtr.t = "ocaml_malloc"
external c_free : HostPtr.t -> unit = "ocaml_free"

external c_cast_int : HostPtr.t -> int = "ocaml_cast_int"
external c_get_int : HostPtr.t -> int -> int = "ocaml_get_int"
external c_set_int : HostPtr.t -> int -> int -> unit = "ocaml_set_int"

external c_cast_int32 : HostPtr.t -> Int32.t = "ocaml_cast_int32"
external c_get_int32 : HostPtr.t -> int -> Int32.t = "ocaml_get_int32"
external c_set_int32 : HostPtr.t -> int -> Int32.t -> unit = "ocaml_set_int32"

external c_cast_float : HostPtr.t -> float = "ocaml_cast_double"
external c_get_float : HostPtr.t -> int -> float = "ocaml_get_double"
external c_set_float : HostPtr.t -> int -> float -> unit = "ocaml_set_double"

(*let int_to_host i = {ptr= HostPtr.of_int i; len=0; host_t=IntT; } *)
let int32_to_host i =
    {ptr=Int64.of_int32 i; 
     host_t=DynType.Int32T; 
     nbytes=4; 
     shape=Shape.empty}
let int64_to_host i =
    {ptr= i;  
     host_t=DynType.Int64T; 
     nbytes=8; 
     shape=Shape.empty}
let float32_to_host f =
    {ptr=Int64.of_int32 $ Float32.bits32_of_float f; 
     shape=Shape.empty; 
     host_t=DynType.Float32T; 
     nbytes=4}
let float64_to_host f =
    {ptr=Int64.bits_of_float f; 
     shape=Shape.empty; 
     host_t=DynType.Float64T; 
     nbytes=8}
    
let bool_to_host b =
    {ptr=Int64.of_bool b; 
     host_t=DynType.BoolT; 
     nbytes=2; 
     shape=Shape.empty}
let host_unit =
    {ptr=0L; 
     host_t=DynType.UnitT; 
     nbytes=1; 
     shape=Shape.empty}

let free h = c_free h.ptr

let mk_scalar = function 
  | PQNum.Int32 i -> int32_to_host i
  | PQNum.Int64 i -> int64_to_host i 
  | PQNum.Float32 f -> float32_to_host f 
  | PQNum.Float64 f -> float64_to_host f
  | PQNum.Bool b -> bool_to_host b 
  | _ -> failwith "[host_val->mk_scalar] not implemented"