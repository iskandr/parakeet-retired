type symbolic_cost = Imp.exp_node

(* returns single expression computing call cost as a function of *)
(* input argument shapes *) 

val symbolic_seq_cost : FnTable.t -> SSA.fundef -> symbolic_cost 

val seq_cost : FnTable.t -> SSA.fundef -> Shape.t list -> float 