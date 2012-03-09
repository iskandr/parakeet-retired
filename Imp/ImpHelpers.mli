open Imp



(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
val wrap_bool : value -> value_node
val wrap_char : value -> value_node
val wrap_int16 : value -> value_node
val wrap_int32 : value -> value_node
val wrap_int64 : value -> value_node
val wrap_float32 : value -> value_node
val wrap_float64 : value -> value_node


val common_type : ?t:Type.elt_t -> value_node list -> ImpType.t

val typed_op : Prim.scalar_op -> ?t:Type.elt_t -> value_node list -> value_node

(* Same as typed_op, but with comparison operators which always return bools *)
val cmp_op : Prim.scalar_op -> ?t:Type.elt_t -> value_node list -> value_node

(* CUDA stuff *)
type vec3 = { x: value_node; y: value_node; z: value_node}
val mk_vec3 : (coord -> value_node) -> vec3

val threadIdx : vec3
val blockIdx : vec3
val blockDim : vec3
val gridDim : vec3

(* GENERAL IMP EXPRESSIONS *)

val int32 : Int32.t -> value_node

val int : int -> value_node
val zero : value_node
val one : value_node

val ints_til : int -> value_node list

(*val infinity : value_node*)

val float : float -> value_node
val double : float -> value_node
val bool : bool -> value_node

val select : value_node -> value_node -> value_node -> value_node

val dim : value_node -> value_node -> value_node
val len : value_node -> value_node
val vec_slice : value_node -> int -> value_node ->  value_node

val max_no_simplify : ?t:Type.elt_t -> value_node -> value_node -> value_node
val min_no_simplify : ?t:Type.elt_t -> value_node -> value_node -> value_node

val mul : value_node -> value_node -> value_node
val add : value_node -> value_node -> value_node
val div : value_node -> value_node -> value_node
val sub : value_node -> value_node -> value_node


val ( *$ ) : value_node -> value_node -> value_node
val ( +$ ):  value_node -> value_node -> value_node
val ( /$ ) : value_node -> value_node -> value_node
val ( -$ ) : value_node -> value_node -> value_node

val mod_ : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( %$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val safe_div_ : ?t:Type.elt_t -> value_node -> value_node -> value_node

val lt : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( <$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val lte : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( <=$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val gt : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( >$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val gte : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( >=$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val eq : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( =$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val neq : ?t:Type.elt_t -> value_node -> value_node -> value_node
val ( <>$ ) : ?t:Type.elt_t -> value_node -> value_node -> value_node

val not_ : value_node -> value_node
val (!$) : value_node -> value_node

val and_ : value_node -> value_node -> value_node
val (&&$) : value_node -> value_node -> value_node

val or_ : value_node -> value_node -> value_node
val (||$) : value_node -> value_node -> value_node

val sqrt32 : value_node -> value_node
val sqrt64 : value_node -> value_node

val ln_32 : value_node -> value_node
val ln_64 : value_node -> value_node

val id_of_value : value_node -> ID.t

val var : ty:ImpType.t -> ID.t -> value_node

val is_const_int : value_node -> bool
val get_const_int : value_node -> int

val cmp : Prim.scalar_op -> value_node -> value_node -> value_node
val scalar_op : Prim.scalar_op -> value_node -> value_node -> value_node

val fixdim : arr:value_node -> dim:int -> idx:value_node -> value_node
val idx : value_node -> ?dims:int list -> value_node list -> value_node

val slice :
  arr:value_node -> dim:value_node -> start:value_node -> stop:value_node ->
    value_node
