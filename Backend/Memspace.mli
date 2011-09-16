

type 'a t
val create : (module PTR with type t = 'a) -> 'a t 
