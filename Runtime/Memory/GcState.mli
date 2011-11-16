
open Base 

type t
    
val create : unit -> t 

val pin : t -> Ptr.t -> unit 
val unpin : t -> Ptr.t -> unit 

val add_used_ptr : t -> Ptr.t -> unit

(* any ptrs which fail predicate get moved to free map *)   
val filter_used_ptrs : t -> (Ptr.t -> bool) -> unit 


val add_free_ptr : t -> Ptr.t -> unit 
val find_free_ptr : t -> int -> Ptr.t option 
val iter_free_ptrs : t -> (Ptr.t -> unit) -> unit
val clear_free_ptrs : t -> unit

val pinned_addr_set : t -> Int64.Set.t 