
(* Scheduler chooses on which backend to run code *) 

type value = ArrayId.t Value.t 
type values = value list 

val map : ?axes:int list -> SSA.fn ->  fixed:values ->  values -> values 

val reduce : 
  ?axes:int list ->  SSA.fn -> fixed:values -> ?init:values -> values -> values 

val scan : 
  ?axes:int list -> SSA.fn -> fixed:values ->  ?init:values -> values -> values
    
val all_pairs :
  ?axes:int list -> SSA.fn -> fixed:values -> value -> value -> value   

val array_op : Prim.array_op -> values -> values  
