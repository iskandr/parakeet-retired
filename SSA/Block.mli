type 'a t  

val empty : 'a t
val singleton : 'a -> 'a t
val of_list : 'a list -> 'a t  
val of_array : 'a array -> 'a t
  
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t 

val insert_after : 'a t -> 'a -> 'a t  
val insert_before : 'a -> 'a t -> 'a t
   
val length : 'a t -> int 
val idx : 'a t -> int -> 'a 

val iter_forward : ('a -> unit) -> 'a t -> unit

val iter_backward : ('a -> unit) -> 'a t -> unit 
val fold_forward : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b 
val fold_backward : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b 

val for_all : ('a -> bool) -> 'a t -> bool 
val to_str : ('a -> string) -> 'a t -> string 
