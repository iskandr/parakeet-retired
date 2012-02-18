type dim_op = Mult | Add | Max
type dim =
  | Const of int
  | Dim of ID.t * int
  | Op of dim_op * dim * dim

type t = dim list
type env = t ID.Map.t

val dim_to_str : dim -> string
val to_str : t -> string

val shapes_to_str : t list -> string

val const : int -> dim
val zero : dim
val one : dim
val dim : ID.t -> int -> dim
val add : dim -> dim -> dim
val mult : dim -> dim -> dim
val max_ : dim -> dim -> dim

val simplify_dim : dim -> dim
val simplify_op : dim_op -> dim -> dim -> dim

val max_of_dims : dim list -> dim
val prod_of_dims : dim list -> dim

val scalar : t
val is_scalar : t -> bool

val rank : t -> int
val get_dim : t -> int -> dim
val get_dims : t -> int list -> dim list
val outer_dim : t -> dim

val argmax_rank : t list -> t * int

val peel_outer_dim : t -> t
val peel : ?axes:int list -> t -> t
val peel_shape_list : ?axes:int list -> t list -> t list




(* combines all dims from shapes of max rank,
   paired with a list of shapes of rank maxRank-1 or lower.
   Example:
   split_max_dim [[2; 4]; [5]]  = 2 * [[4];[5]]
*)
val split_max_rank : t list -> dim * t list


val all_dims : ID.t -> int -> t

(*val largest_ssa_val : SSA.value_node list -> SSA.value_node*)
val of_int_list : int list -> t

val concat : t -> t -> t

val rewrite_dim : t ID.Map.t -> dim -> dim
val rewrite_shape : t ID.Map.t -> t -> t
val rewrite_shapes : t ID.Map.t -> t list -> t list

val split : ?curr_dim:int -> t -> int list -> t * t