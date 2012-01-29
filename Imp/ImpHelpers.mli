open Imp


(* IMP STATEMENTS *)
val syncthreads : stmt
val if_ : value_node -> block -> block -> stmt
val ifTrue : value_node -> block -> stmt
val while_ : exp_node -> block -> stmt
val comment : string -> stmt

val set : value_node -> exp_node -> stmt
val set_val : value_node -> value_node -> stmt
val setidx : value_node -> value_node list -> value_node -> stmt

(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
val wrap_bool_val : value -> value_node
val wrap_char_val : value -> value_node
val wrap_int16_val : value -> value_node
val wrap_int32_val : value -> value_node
val wrap_int64_val : value -> value_node
val wrap_float32_val : value -> value_node
val wrap_float64_val : value -> value_node

val exp_of_val : value_node -> exp_node

val wrap_exp : exp -> ImpType.t -> exp_node
val wrap_bool_exp : exp -> exp_node
val wrap_char_exp : exp -> exp_node
val wrap_int16_exp : exp -> exp_node
val wrap_int32_exp : exp -> exp_node
val wrap_int64_exp : exp -> exp_node
val wrap_float32_exp : exp -> exp_node
val wrap_float64_exp : exp -> exp_node

(*
val typed_exp : Type.elt_t -> exp -> exp_node
val bool_exp : exp->exp_node
val int16_exp : exp->exp_node
val int_exp : exp -> exp_node
val f32_exp : exp -> exp_node
val f64_exp : exp -> exp_node
*)
(* CAST AN EXPRESSION TO A NEW TYPE
   (or leave it alone if it's already that type
*)

val cast : ImpType.t -> value_node -> exp_node

val common_type : ?t:Type.elt_t -> value_node list -> ImpType.t


val typed_op : Prim.scalar_op -> ?t:Type.elt_t -> value_node list -> exp_node

(* Same as typed_op, but with comparison operators which always return bools *)
val cmp_op : Prim.scalar_op -> ?t:Type.elt_t -> value_node list -> exp_node

(* CUDA stuff *)
type vec3 = { x: value_node; y: value_node; z: value_node}
val mk_vec3 : (coord -> value_node) -> vec3

val threadIdx : vec3
val blockIdx : vec3
val blockDim : vec3
val gridDim : vec3

(* GENERAL IMP EXPRESSIONS *)

val int32 : Int32.t -> value_node
val int32_exp : Int32.t -> exp_node

val int : int -> value_node
val int_exp : int -> exp_node

val zero : value_node
val zero_exp : exp_node

val one : value_node
val one_exp : exp_node

(*val infinity : value_node*)

val float : float -> value_node
val float_exp : float -> exp_node

val double : float -> value_node
val double_exp : float -> exp_node

val bool : bool -> value_node
val bool_exp : bool -> exp_node

val select : value_node -> value_node -> value_node -> exp_node
val idx : value_node -> value_node list -> exp_node
val dim : value_node -> value_node -> exp_node
val len : value_node -> exp_node

val max_ : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val min_ : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val mul : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( *$ ) :  ?t:Type.elt_t -> value_node -> value_node -> exp_node

val add : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( +$ ): ?t:Type.elt_t -> value_node -> value_node -> exp_node

val div : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( /$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val sub : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( -$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val mod_ : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( %$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val safe_div_ : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val lt : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( <$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val lte : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( <=$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val gt : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( >$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val gte : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( >=$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val eq : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( =$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node

val neq : ?t:Type.elt_t -> value_node -> value_node -> exp_node
val ( <>$ ) : ?t:Type.elt_t -> value_node -> value_node -> exp_node


val not_ : value_node -> exp_node
val (!$) : value_node -> exp_node

val and_ : value_node -> value_node -> exp_node
val (&&$) : value_node -> value_node -> exp_node

val or_ : value_node -> value_node -> exp_node
val (||$) : value_node -> value_node -> exp_node

val sqrt32 : value_node -> exp_node
val sqrt64 : value_node -> exp_node

val ln_32 : value_node -> exp_node
val ln_64 : value_node -> exp_node

val id_of_val : value_node -> ID.t
val id_of_exp : exp_node -> ID.t

val var : ty:ImpType.t -> ID.t -> value_node
val var_exp : ty:ImpType.t -> ID.t -> exp_node

(*
val max_simplify : exp_node -> exp_node -> exp_node
val mul_simplify : exp_node -> exp_node -> exp_node
val add_simplify : exp_node -> exp_node -> exp_node

val max_exp_node_list : exp_node list -> exp_node
val prod_exp_node_list : exp_node list -> exp_node
val sum_exp_node_list : exp_node list -> exp_node
*)
(*
val highest_rank_exp :  exp_node array -> exp_node
*)
