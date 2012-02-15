
(* returns single expression computing call cost as a function of *)
(* input argument shapes *) 

val symbolic_seq_cost : FnTable.t -> SSA.fundef -> Imp.exp_node 

val seq_cost : FnTable.t -> SSA.fundef -> Shape.t list -> float 