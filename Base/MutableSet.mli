type 'a t 
val create : int -> 'a t 
val mem : 'a t -> 'a -> bool
val add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit
val enum : 'a t -> 'a Enum.t
val iter : ('a -> unit) -> 'a t ->   unit 
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b 