
(*
type setters = { 
		set_bool : (Int64.t -> int -> bool -> unit); 
    set_char : (Int64.t -> int -> char -> unit); 
    set_int32 : (Int64.t -> int -> Int32.t -> unit);  
    set_int64 : (Int64.t -> int -> Int64.t -> unit);  
    set_float32 : (Int64.t -> int -> float -> unit); 
    set_float64 : (Int64.t -> int -> float -> unit);
}
*)
type raw_fns = { 
    alloc : (int -> Int64.t); 
    delete : (Int64.t -> unit); 
   	get_bool : (Int64.t -> int -> bool); 
    get_char : (Int64.t -> int -> char); 
    get_int32 : (Int64.t -> int -> Int32.t);  
    get_int64 : (Int64.t -> int -> Int64.t);  
    get_float32 : (Int64.t -> int -> float); 
    get_float64 : (Int64.t -> int -> float);
}

type t = { 
    addr : Int64.t; 
    size : int; 
    memspace : MemId.t;
    fns: raw_fns;  
}

let addr {addr} = addr 
let size {size} = size 
let memspace {memspace} = memspace 


let to_str {addr;size;memspace} =
    let name = MemId.find_name memspace in   
    Printf.sprintf "<ptr addr=%Ld, size=%d, memspace=%s" addr size name

let get_bool ptr offset = ptr.fns.get_bool ptr.addr offset 
let get_char ptr offset = ptr.fns.get_char ptr.addr offset
let get_int32 ptr offset = ptr.fns.get_int32 ptr.addr offset
let get_int64 ptr offset = ptr.fns.get_int64 ptr.addr offset   
let get_float32 ptr offset = ptr.fns.get_float32 ptr.addr offset 
let get_float64 ptr offset =  ptr.fns.get_float64 ptr.addr offset 
