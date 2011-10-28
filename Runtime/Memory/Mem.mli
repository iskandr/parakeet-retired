
(* Takes a memspace's memory management functionality and *)
(*   1) wraps allocation with garbage collector book-keeping*)
(*   2) present a uniform pointer type across all memspaces *)
(*   3) performs dispatch on id to call coorect functions for each pointer. *)


type memspace_id 

type ptr = { 
    addr : Int64.t; 
    size : int; 
    memspace : memspace_id
}

type memspace_functions = { 
    alloc : (int -> Int64.t); 
    free : (Int64.t -> unit); 
    idx_bool : (Int64.t -> int -> bool); 
    idx_char : (Int64.t -> int -> char); 
    idx_int32 : (Int64.t -> int -> Int32.t);  
    idx_int64 : (Int64.t -> int -> Int64.t);  
    idx_float32 : (Int64.t -> int -> float); 
    idx_float64 : (Int64.t -> int -> float); 
} 

val register : string -> memspace_functions -> memspace_id 
val memspace_name : memspace_id -> string 

val alloc : int -> ptr
val unsafe_free : ptr -> unit  

val idx_bool : ptr -> int -> bool 
val idx_char : ptr -> int -> char  
val idx_int32 : ptr -> int -> Int32.t   
val idx_int64 : ptr -> int -> Int64.t   
val idx_float32 : ptr -> int -> float  
val idx_float64 : ptr -> int -> float 

val trace_free_ptrs : memspace_id -> unit 
val delete_free_ptrs : memspace_id -> unit

val pin : ptr -> unit
val unpin : ptr -> unit 
 