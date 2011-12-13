open Ptr

external c_malloc_impl : int -> Int64.t = "ocaml_malloc"

exception HostOutOfMemory
let malloc nbytes =
  let ptr = c_malloc_impl nbytes in
  if ptr = Int64.zero then raise HostOutOfMemory
  else ptr

external free : Int64.t -> unit = "ocaml_free"
external memcpy : Int64.t -> Int64.t -> int -> unit = "ocaml_memcpy"    
    
external get_array1_ptr
  : ('a,'b,'c) Bigarray.Array1.t -> Int64.t = "get_bigarray_ptr"

external get_array2_ptr
  : ('a,'b,'c) Bigarray.Array2.t -> Int64.t = "get_bigarray_ptr"

external get_array3_ptr
  : ('a,'b,'c) Bigarray.Array3.t -> Int64.t = "get_bigarray_ptr"

external get_int32 : Int64.t -> int -> Int32.t = "ocaml_get_int32"
external set_int32 : Int64.t -> int -> Int32.t -> unit = "ocaml_set_int32"

external get_int64 : Int64.t -> int -> Int64.t = "ocaml_get_int64"
external set_int64 : Int64.t -> int -> Int64.t -> unit = "ocaml_set_int64"

external get_float32 : Int64.t -> int -> float = "ocaml_get_float"
external set_float32 : Int64.t -> int -> float -> unit = "ocaml_set_float"

external get_float64 : Int64.t -> int -> float = "ocaml_get_double"
external set_float64 : Int64.t -> int -> float -> unit = "ocaml_set_double"

external get_char : Int64.t -> int -> char = "ocaml_get_char"
external set_char : Int64.t -> int -> char -> unit = "ocaml_set_char"

let get_bool p offset  = Char.code (get_char p offset) > 0
let set_bool p offset b = set_char p offset (Char.chr (if b then 1 else 0))
 
let fns : Ptr.raw_fns = {
    Ptr.alloc = malloc;
    delete = free;
    get_bool = get_bool;
    get_char = get_char;
    get_int32 = get_int32;
    get_int64 = get_int64;
    get_float32 = get_float32;
    get_float64 = get_float64;
}

let id = Mem.register "host" fns

let mk_host_ptr ptr size =
  DataManager.register_ptr {addr=ptr; size=size; memspace=id; fns=fns}
