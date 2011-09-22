


type 'a memspace
val register_memspace : (module PTR with type t = 'a) -> 'a memspace 

type t 

val box : 'a memspace -> 'a -> t 
val unbox : 'a memspace -> t -> 'a option 

val free : t -> unit 