val call : TypedSSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list

val adverb :
  (TypedSSA.fn, Ptr.t Value.t list, int list) Adverb.t -> Ptr.t Value.t list

val set_multithreading : bool -> unit
