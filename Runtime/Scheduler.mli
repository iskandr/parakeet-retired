
(* Scheduler chooses on which backend to run code *) 

val map :  
    SSA.fn -> fixed:Value.t list -> Value.t list -> Value.t list

val reduce : 
    SSA.fn -> ?init:Value.t list -> fixed:Value.t list -> Value.t list -> Value.t list 

val scan : 
    SSA.fn -> ?init:Value.t list -> fixed:Value.t list -> Value.t list -> Value.t list
    
val all_pairs : SSA.fn -> fixed:Value.t list -> Value.t -> Value.t -> Value.t  

val array_op : Prim.array_op -> Value.t list -> Value.t list 
