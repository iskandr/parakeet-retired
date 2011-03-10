
(* given a shape environment, evaluate a single imp expression*)
(* which is expected to only refer to variables by their dimsize *) 
val eval_exp_as_int :  Shape.t ID.Map.t -> Imp.exp_node -> int 
val eval_exp_as_float :  Shape.t ID.Map.t -> Imp.exp_node -> float

(* returns the shapes of every variable in the function body *) 
val eval_imp_shape_env : Imp.fn -> Shape.t list -> Shape.t ID.Map.t
 
(* returns only the output shapes of SSA function *) 
val eval_ssa_output_shapes : 
      FnTable.t -> SSA.fundef -> Shape.t list -> Shape.t list
       
val eval_ssa_shape_env 
      : FnTable.t -> SSA.fundef -> Shape.t list -> Shape.t ID.Map.t  
      