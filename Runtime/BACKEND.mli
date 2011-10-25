

module Memspace : MEMSPACE 
   
val map : 
  SSA.fn -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val reduce : 
  SSA.fn -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val scan : 
  SSA.fn -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val array_op : Prim.array_op -> Data.t list -> Data.t list             