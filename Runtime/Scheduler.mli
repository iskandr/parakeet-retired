
(* Scheduler chooses on which backend to run code *) 

val map : ?axes:int list -> SSA.fn ->  fixed:Value.t list -> 
  Value.t list -> Value.t list

val reduce : ?axes:int list ->  SSA.fn -> fixed:Value.t list -> 
  ?init:Value.t list -> Value.t list -> Value.t list 

val scan : ?axes:int list -> SSA.fn -> fixed:Value.t list ->  
  ?init:Value.t list -> Value.t list -> Value.t list
    
val all_pairs : SSA.fn ->   fixed:Value.t list ->  ?axes:int list -> 
    Value.t -> Value.t -> Value.t  

val array_op : Prim.array_op -> Value.t list -> Value.t list 
