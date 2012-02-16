(* evaluates a function applied to a set of arguments on the host *)
val call : SSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list