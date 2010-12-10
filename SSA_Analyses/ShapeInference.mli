
type shape_env = Shape.t ID.Map.t

val infer_fundef :
  FnTable.t -> SSA.fundef -> Shape.t list -> shape_env   
  
(* adverbs return the shape of their outputs and the shape environment
   of their function argument. 
*)
val infer_map : 
  FnTable.t -> SSA.fundef -> Shape.t list -> Shape.t list * shape_env  
  
val infer_allpairs : 
  FnTable.t -> SSA.fundef -> Shape.t list -> Shape.t list * shape_env 
  
val infer_reduce : 
  FnTable.t -> SSA.fundef -> Shape.t list -> Shape.t list * shape_env    
 
  