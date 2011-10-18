
val eval_app : SSA.fn -> Value.t list -> Value.t list 

(* evaluates a function applied to a set of arguments on the host *) 
val eval_host : SSA.fundef ->  Data.t list -> Data.t list
