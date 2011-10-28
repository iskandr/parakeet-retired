
type raw_fns = { 
    alloc : (int -> Int64.t); 
    delete : (Int64.t -> unit); 
    idx_bool : (Int64.t -> int -> bool); 
    idx_char : (Int64.t -> int -> char); 
    idx_int32 : (Int64.t -> int -> Int32.t);  
    idx_int64 : (Int64.t -> int -> Int64.t);  
    idx_float32 : (Int64.t -> int -> float); 
    idx_float64 : (Int64.t -> int -> float); 
}

type t = { 
    addr : Int64.t; 
    size : int; 
    memspace : MemId.t;
    fns: raw_fns;  
}

let to_str {addr;size;memspace} =
    let name = MemId.find_name memspace in   
    Printf.sprintf "<ptr addr=%Ld, size=%d, memspace=%s" addr size name

let idx_bool ptr offset = ptr.fns.idx_bool ptr.addr offset 
let idx_char ptr offset = ptr.fns.idx_char ptr.addr offset
let idx_int32 ptr offset = ptr.fns.idx_int32 ptr.addr offset
let idx_int64 ptr offset = ptr.fns.idx_int64 ptr.addr offset   
let idx_float32 ptr offset = ptr.fns.idx_float32 ptr.addr offset 
let idx_float64 ptr offset =  ptr.fns.idx_float64 ptr.addr offset 
