
val call : SSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list

val map :
  axes:int list -> fn:SSA.fn -> fixed:Ptr.t Value.t list ->
    Ptr.t Value.t list -> Ptr.t Value.t list

val reduce : axes:int list -> fn:SSA.fn -> fixed:Ptr.t Value.t list ->
   ?init:Ptr.t Value.t list ->  Ptr.t Value.t list -> Ptr.t Value.t list

val scan : axes:int list -> fn:SSA.fn -> fixed:Ptr.t Value.t list ->
  ?init:Ptr.t Value.t list -> Ptr.t Value.t list -> Ptr.t Value.t list

val allpairs : axes:int list -> fn:SSA.fn -> fixed:Ptr.t Value.t list ->
  Ptr.t Value.t -> Ptr.t Value.t -> Ptr.t Value.t list
