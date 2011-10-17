

module Memspace : MEMSPACE 
   
val map : 
  SSA.fundef -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val reduce : 
  SSA.fundef -> Data.t list -> Data.t list -> axes:int list -> Data.t list

val scan : 
  SSA.fundef -> Data.t list -> Data.t list -> axes:int list -> Data.t list

    
            