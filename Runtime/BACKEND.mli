
type data = Ptr.t Value.t 

val memspace_id : MemId.t  
   
val map :  axes:int list -> fn:SSA.fn -> fixed:(data list) -> 
    data list -> data list

val reduce : axes:int list -> fn:SSA.fn -> fixed:(data list) -> 
    ?init:data list -> data list -> data list

val scan : axes:int list -> fn:SSA.fn -> fixed:(data list) -> 
    ?init:data list -> data list -> data list

val all_pairs : axes:int list -> fn:SSA.fn -> fixed:(data list) -> 
    data -> data -> data list

val array_op : Prim.array_op -> data list -> data list