(* pulled out of Mem.ml to break recursion between Mem and DataManager *) 

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
    (* keep the memspace id so we can lookup memspace-specific*)
    (* metadata like the garbage collector's state *) 
    memspace : MemId.t;
    (* dispatch table for generic functions below: *) 
    fns : raw_fns;  
}

val to_str : t -> string 

val idx_bool : Ptr.t -> int -> bool 
val idx_char : Ptr.t -> int -> char  
val idx_int32 : Ptr.t -> int -> Int32.t   
val idx_int64 : Ptr.t -> int -> Int64.t   
val idx_float32 : Ptr.t -> int -> float  
val idx_float64 : Ptr.t -> int -> float 