
module Data : sig type t end 

module Memspace : MEMSPACE 

val map_cost : 
  Shape.t ID.Map.t -> SSA.closure -> SSA.value list -> axes:int list -> float 

val scan_cost : 
  Shape.t ID.Map.t -> SSA.closure -> SSA.value list -> axes:int list -> float 

val reduce_cost : 
  Shape.t ID.Map.t -> SSA.closure -> SSA.value list -> axes:int list -> float 
   
val map : 
  SSA.fundef -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val reduce : 
  SSA.fundef -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val scan : 
  SSA.fundef -> Data.t list -> Data.t list -> axes:int list -> Data.t list

    
            