type dim_op = Mult | Add | Max 
type dim = 
  | Const of int 
  | Dim of ID.t * int 
  | Op of dim_op * dim * dim 
  
type shape = dim list 
type env = shape ID.Map.t 

val dim_to_str : dim -> string 
val to_str : shape -> string

val shapes_to_str : shape list -> string 

val const : int -> dim 
val zero : dim 
val one : dim 
val dim : ID.t -> int -> dim 
val add : dim -> dim -> dim 
val mult : dim -> dim -> dim 
val max_ : dim -> dim -> dim 

val max_of_dims : dim list -> dim 
val prod_of_dims : dim list -> dim 

val scalar : shape
val is_scalar : shape -> bool 

val rank : shape -> int 
val get_dim : shape -> int -> dim 
val outer_dim : shape -> dim 

val peel_outer_dim : shape -> shape
val peel : ?axes:int list -> shape -> shape   
val peel_shape_list : ?axes:int list -> shape list -> shape list 
    
val split_shape : shape -> dim * shape
val split_shape_list : shape list -> dim list * shape list 


(* combines all dims from shapes of max rank,
   paired with a list of shaImp.pes of rank maxRank-1 or lower. 
   Example: 
   split_max_dim [[2; 4]; [5]]  = 2 * [[4];[5]]
*)
val split_max_rank : shape list -> dim * shape list  


val all_dims : ID.t -> int -> shape 
 
(*val largest_ssa_val : SSA.value_node list -> SSA.value_node*) 
val of_int_list : int list -> shape 
 
val concat : shape -> shape -> shape

val rewrite_dim : shape ID.Map.t -> dim -> dim
val rewrite_shape : shape ID.Map.t -> shape -> shape
val rewrite_shapes : shape ID.Map.t -> shape list -> shape list 
    
 


   


  
