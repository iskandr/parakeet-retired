
type optimization = FnTable.t -> SSA.fundef -> SSA.fundef * bool 

val optimize_fundef :  
  ?maxiters:int -> FnTable.t -> SSA.fundef -> 
    (string*optimization) list -> SSA.fundef * int
    
val optimize_all_fundefs :
  ?maxiters:int ->  FnTable.t -> (string*optimization) list  -> unit 
    