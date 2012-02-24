val call : TypedSSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list

val adverb :
  (TypedSSA.fn, Ptr.t Value.t list, int list) Adverb.info ->
    Ptr.t Value.t list ->
      Ptr.t Value.t list

(*
val map :
  axes:int list -> fn:TypedSSA.fn -> fixed:Ptr.t Value.t list ->
  Ptr.t Value.t list -> Ptr.t Value.t list

val reduce : axes:int list -> fn:TypedSSA.fn -> fixed:Ptr.t Value.t list ->
  ?init:Ptr.t Value.t list -> Ptr.t Value.t list -> Ptr.t Value.t list

val scan : axes:int list -> fn:TypedSSA.fn -> fixed:Ptr.t Value.t list ->
  ?init:Ptr.t Value.t list -> Ptr.t Value.t list -> Ptr.t Value.t list

val allpairs : axes:int list -> fn:TypedSSA.fn -> fixed:Ptr.t Value.t list ->
  Ptr.t Value.t -> Ptr.t Value.t -> Ptr.t Value.t list
*)