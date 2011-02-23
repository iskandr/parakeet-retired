

(* returns the shapes of every variable in the function body *) 
val eval_imp_shape_env : Imp.fn -> Shape.t list -> Shape.t ID.Map.t
 
(* returns only the output shapes of SSA function *) 
val eval_ssa_output_shapes : 
      FnTable.t -> SSA.fundef -> Shape.t list -> Shape.t list 