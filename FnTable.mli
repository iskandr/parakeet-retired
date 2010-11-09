type t 

val add :  SSA.fundef -> t -> unit 
val find : SSA.FnId.t -> t -> SSA.fundef   
val find_option : SSA.FnId.t -> t -> SSA.fundef option 
val mem : SSA.FnId.t -> t -> bool 
val from_list : SSA.fundef list -> t 
val create :  int -> t  
val have_unoptimized : t -> bool 
val get_unoptimized : t -> SSA.fundef 
val update : SSA.fundef -> t -> unit 
 
val get_fundef : t -> SSA.value_node -> SSA.fundef