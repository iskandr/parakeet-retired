
type optimization = FnTable.t -> SSA.fn -> SSA.fn * bool 

val optimize_fn :  
  ?type_check:bool -> ?iter:int -> ?maxiters:int -> FnTable.t -> SSA.fn -> 
    (string*optimization) list -> SSA.fn * int
    
val optimize_all_fns :
  ?type_check:bool -> ?maxiters:int ->  
    FnTable.t -> (string*optimization) list  -> unit 
    