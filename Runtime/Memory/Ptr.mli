(* pulled out of Mem.ml to break recursion between Mem and DataManager *) 

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
    (* keep the memspace id so we can lookup memspace-specific*)
    (* metadata like the garbage collector's state *) 
    memspace : MemId.t;
    (* dispatch table for generic functions below: *) 
    fns : raw_fns;  
}

val addr : t -> Int64.t 
val size : t -> int 
val memspace : t -> MemId.t 
val to_str : t -> string 

val get_bool : t -> int -> bool 
val get_char : t -> int -> char  
val get_int32 : t -> int -> Int32.t   
val get_int64 : t -> int -> Int64.t   
val get_float32 : t -> int -> float  
val get_float64 : t -> int -> float 