
val apply_exp_map : Imp.exp_node ID.Map.t  -> Imp.exp_node -> Imp.exp_node 
val apply_exp_map_to_stmt : Imp.exp_node ID.Map.t  -> Imp.stmt -> Imp.stmt 
  
val apply_id_map : ID.t ID.Map.t -> Imp.exp_node -> Imp.exp_node  
val apply_id_map_to_stmt : ID.t ID.Map.t -> Imp.stmt -> Imp.stmt