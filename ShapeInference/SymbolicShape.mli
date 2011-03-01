type dim = Imp.exp_node
type shape = Imp.exp_node list 
type env = shape ID.Map.t 

val scalar : shape

val rank : shape -> int 

 
val get_dim : shape -> int -> dim 
val outer_dim : shape -> dim 

val peel_shape : shape -> shape  
val peel_shape_list : shape list -> shape list 

val split_shape : shape -> dim * shape
val split_shape_list : shape list -> dim list * shape list 


val max_dim : dim -> dim -> dim
val max_dim_of_list : dim list -> dim


(* combines all dims from shapes of max rank,
   paired with a list of shapes of rank maxRank-1 or lower. 
   Example: 
   split_max_dim [[2; 4]; [5]]  = 2 * [[4];[5]]
*)
val split_max_rank : shape list -> dim * shape list  

val shape_to_str : shape -> string 
val shapes_to_str : shape list -> string 
val all_dims : Imp.exp_node -> shape 
 
val largest_val : Imp.exp_node array ->  Imp.exp_node
val of_int_list : int list -> shape 

val rewrite_dim : shape ID.Map.t -> dim -> dim
val rewrite_shape : shape ID.Map.t -> shape -> shape

val nelts : shape -> dim    
val to_str : shape -> string 