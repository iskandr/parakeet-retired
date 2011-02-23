type dim = Imp.exp_node
type shape = Imp.exp_node list 

val scalar : shape 
val peel_shape : shape -> shape  
val peel_shape_list : shape list -> shape list 
val split_shape : shape -> dim * shape
val split_shape_list : shape list -> dim list * shape list 
val rank : shape -> int 
val mk_max_dim : shape -> dim

val shape_to_str : shape -> string 
val shapes_to_str : shape list -> string 
val all_dims : Imp.exp_node -> shape 
 
val largest_val : Imp.exp_node array ->  Imp.exp_node
val of_int_list : int list -> shape 

val rewrite_dim : shape ID.Map.t -> dim -> dim
val rewrite_shape : shape ID.Map.t -> shape -> shape  