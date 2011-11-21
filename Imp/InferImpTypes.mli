type tenv = ImpType.t ID.Map.t  
val infer_value : tenv -> SSA.value_node -> tenv 
val infer_exp : tenv -> SSA.exp_node -> tenv  
val infer_stmt : tenv -> SSA.stmt_node -> tenv 
val infer : SSA.fn -> ImpType.t list -> tenv 
    
          