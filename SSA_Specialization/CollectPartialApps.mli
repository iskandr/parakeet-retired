open SSA 

type closure_env = { 
  closures : (ID.t, value) Hashtbl.t; 
  closure_args : (ID.t, ID.t list) Hashtbl.t;
  closure_arity : (ID.t, int) Hashtbl.t; 
}
 
val collect_partial_apps 
   : InterpState.t -> SSA.fundef -> SSA.fundef * closure_env