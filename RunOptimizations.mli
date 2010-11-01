
type optimization = FnTable.t -> SSA.fundef -> SSA.fundef * bool 

val optimize_fundef :  
  ?maxiters:int -> ?inline:bool -> 
    FnTable.t -> SSA.fundef -> (string*optimization) list -> SSA.fundef 
    