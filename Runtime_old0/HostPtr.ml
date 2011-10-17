type t = Int64.t

external c_get_int32 : Int64.t -> int -> Int32.t = "ocaml_get_int32"
external c_set_int32 : Int64.t -> int -> Int32.t -> unit = "ocaml_set_int32"
let get_int32 p ?(offset=0) = c_get_int32 p offset 
let set_int32 p ?(offset=0) value = c_set_int32 p offset value

external c_get_float32 : Int64.t -> int -> float = "ocaml_get_float"
external c_set_float32 : Int64.t -> int -> float -> unit = "ocaml_set_float"
let get_float32 p ?(offset=0) = c_get_float32 p offset
let set_float32 p ?(offset=0) value = c_set_float32 p offset value 

external c_get_float64 : Int64.t -> int -> float = "ocaml_get_double"
external c_set_float64 : Int64.t -> int -> float -> unit = "ocaml_set_double"
let get_float64 p ?(offset=0) = c_get_float64 p offset
let set_float64 p ?(offset=0) value = c_set_float64 p offset value 

external c_get_char : Int64.t -> int -> int = "ocaml_get_char"
external c_set_char : Int64.t -> int -> int -> unit = "ocaml_set_char"
let get_char p ?(offset=0) = c_get_char p offset
let set_char p ?(offset=0) value = c_set_char p offset value 

let get_bool p ?(offset=0) = (c_get_char p offset) > 0  
let set_bool p ?(offset=0) b = c_set_char p offset (if b then 1 else 0) 
