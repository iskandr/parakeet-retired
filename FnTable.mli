type t 

val add :  SSA.fundef -> t -> SSA.FnId.t
val find : SSA.FnId.t -> t -> SSA.fundef   
val find_option : SSA.FnId.t -> t -> SSA.fundef option 
val mem : SSA.FnId.t -> t -> bool 
val from_list : SSA.fundef list -> t 
val create :  int -> t  