type t 
val add : ?opt_queue:bool -> SSA.fundef -> t -> unit 
val find : FnId.t -> t -> SSA.fundef   
val find_option : FnId.t -> t -> SSA.fundef option 
val mem : FnId.t -> t -> bool 
val from_list : SSA.fundef list -> t 
val create :  int -> t  
val have_unoptimized : t -> bool 
val get_unoptimized : t -> SSA.fundef 
val update : SSA.fundef -> t -> unit 
val get_fundef : t -> SSA.value_node -> SSA.fundef