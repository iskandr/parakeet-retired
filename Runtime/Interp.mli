type value = DataId.t Value.t

val eval_app : SSA.fn -> value list -> value list
val eval_exp : SSA.exp_node -> value list

(* evaluates a function applied to a set of arguments on the host *)
val run : SSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list
