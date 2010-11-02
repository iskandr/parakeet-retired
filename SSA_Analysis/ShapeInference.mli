
type required_allocs = { 
  output_shapes : Shape.t list;
  output_types : DynType.t list;    
  local_shapes : Shape.t list;
  local_types : DynType.t list;  
}

val infer_fundef :
  FnTable.t -> SSA.fundef -> Shape.t list -> required_allocs 
  
val infer_map : 
  FnTable.t -> SSA.fundef -> Shape.t list -> required_allocs
  
val infer_allpairs : 
  FnTable.t -> SSA.fundef -> Shape.t list -> required_allocs 
  
val infer_reduce : 
  FnTable.t -> SSA.fundef -> Shape.t list -> required_allocs   
 
  