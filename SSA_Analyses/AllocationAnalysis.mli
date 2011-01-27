


(* map every non-scalar variable in the function to whether it needs
   to be explicitly allocated or can simply be realized as a pointer
   into some other heap structure
*) 
val infer_fundef : SSA.fundef -> ID.Set.t 