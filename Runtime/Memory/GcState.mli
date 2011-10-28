

type t
    
val create : unit -> t 

val pin : t -> Ptr.t -> unit 
val unpin : t -> Ptr.t -> unit 

val add_ptr : t -> Ptr.t -> unit  

val add_free_ptr : t -> Ptr.t -> unit 
val find_free_ptr : t -> int -> Ptr.t option 
