type t 
val add : ?opt_queue:bool -> SSA.fn -> t -> unit 
val find : FnId.t -> t -> SSA.fn   
val find_option : FnId.t -> t -> SSA.fn option 
val mem : FnId.t -> t -> bool 
val from_list : SSA.fn list -> t 
val create :  int -> t  
val have_unoptimized : t -> bool 
val get_unoptimized : t -> SSA.fn 
val update : SSA.fn -> t -> unit 
val get_fundef : t -> SSA.value_node -> SSA.fn
val get_arity :  FnId.t -> t -> int 