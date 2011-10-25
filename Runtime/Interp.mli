

  
val eval_app : SSA.fn -> Value.t list -> Value.t list 
val eval_exp :  SSA.exp_node -> Value.t list

(* evaluates a function applied to a set of arguments on the host *) 
val run : SSA.fn ->  Data.t list -> Data.t list
