
type value = ArrayId.t Value.t 
  
val eval_app : SSA.fn -> value list -> value list 
val eval_exp :  SSA.exp_node -> value list

(* evaluates a function applied to a set of arguments on the host *) 
val run : SSA.fn ->  Array.t list -> Array.t list
