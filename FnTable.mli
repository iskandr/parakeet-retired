type t 

val add :  ?id:ID.t -> SSA.fundef -> t -> ID.t
  
val find : ID.t -> t -> SSA.fundef   
val find_option : ID.t -> t -> SSA.fundef option 
val mem : ID.t -> t -> bool 
val from_list : (ID.t * SSA.fundef) list -> t 
val create :  int -> t  