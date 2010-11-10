
type optimization = FnTable.t -> SSA.fundef -> SSA.fundef * bool 

val optimize_fundef :  
  ?type_check:bool -> ?iter:int -> ?maxiters:int -> FnTable.t -> SSA.fundef -> 
    (string*optimization) list -> SSA.fundef * int
    
val optimize_all_fundefs :
  ?type_check:bool -> ?maxiters:int ->  
    FnTable.t -> (string*optimization) list  -> unit 
    