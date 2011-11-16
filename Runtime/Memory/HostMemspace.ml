


external get_int32 : Int64.t -> int -> Int32.t = "ocaml_get_int32"
external set_int32 : Int64.t -> int -> Int32.t -> unit = "ocaml_set_int32"

external get_float32 : Int64.t -> int -> float = "ocaml_get_float"
external set_float32 : Int64.t -> int -> float -> unit = "ocaml_set_float"

external get_float64 : Int64.t -> int -> float = "ocaml_get_double"
external set_float64 : Int64.t -> int -> float -> unit = "ocaml_set_double"

external get_char : Int64.t -> int -> int = "ocaml_get_char"
external set_char : Int64.t -> int -> int -> unit = "ocaml_set_char"

let get_bool p offset  = (get_char p offset) > 0  
let set_bool p offset b = set_char p offset (if b then 1 else 0) 


let fns : Ptr.raw_fns = { 
    alloc = malloc;  
    delete = delete;  
    get_bool = get_char;  
    get_char = get_char;  
    get_int32 = get_int32;   
    get_int64 = get_int64;   
    get_float32 = get_float32;  
    get_float64 = get_float64;  
}

let id = Mem.register "host" fns  
