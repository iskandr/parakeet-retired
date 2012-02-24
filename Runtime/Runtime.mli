(* evaluates a function applied to a set of arguments on the host *)
val call : TypedSSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list