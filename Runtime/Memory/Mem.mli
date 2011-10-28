
(* Takes a memspace's memory management functionality and *)
(*   1) wraps allocation with garbage collector book-keeping*)
(*   2) present a uniform pointer type across all memspaces *)
(*   3) performs dispatch on id to call coorect functions for each pointer. *)



type memspace_fns = { 
    alloc : (int -> Int64.t); 
    free : (Int64.t -> unit); 
    idx_bool : (Int64.t -> int -> bool); 
    idx_char : (Int64.t -> int -> char); 
    idx_int32 : (Int64.t -> int -> Int32.t);  
    idx_int64 : (Int64.t -> int -> Int64.t);  
    idx_float32 : (Int64.t -> int -> float); 
    idx_float64 : (Int64.t -> int -> float); 
} 

val register : string -> memspace_fns -> MemId.t 

val alloc : int -> Ptr.t
val unsafe_free : Ptr.t -> unit  

val idx_bool : Ptr.t -> int -> bool 
val idx_char : Ptr.t -> int -> char  
val idx_int32 : Ptr.t -> int -> Int32.t   
val idx_int64 : Ptr.t -> int -> Int64.t   
val idx_float32 : Ptr.t -> int -> float  
val idx_float64 : Ptr.t -> int -> float 

val trace_free_ptrs : MemId.t -> unit 
val delete_free_ptrs : MemId.t -> unit

val pin : Ptr.t -> unit
val unpin : Ptr.t -> unit 
 