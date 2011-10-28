
(* Takes a memspace's memory management functionality and *)
(*   1) wraps allocation with garbage collector book-keeping*)
(*   2) present a uniform pointer type across all memspaces *)
(*   3) performs dispatch on id to call coorect functions for each pointer. *)



val register : string -> Ptr.raw_fns -> MemId.t 

val alloc : int -> Ptr.t
val unsafe_free : Ptr.t -> unit  


val trace_free_ptrs : MemId.t -> unit 
val delete_free_ptrs : MemId.t -> unit

val pin : Ptr.t -> unit
val unpin : Ptr.t -> unit 
 