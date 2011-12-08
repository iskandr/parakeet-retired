open Imp 

(* IMP STATEMENTS *)
val syncthreads : stmt
val if_ : exp_node -> block -> block -> stmt 
val ifTrue : exp_node -> block -> stmt   
val while_ : exp_node -> block -> stmt 
val comment : string -> stmt 

val set : exp_node -> exp_node -> stmt 
val setidx : exp_node -> exp_node list -> exp_node -> stmt 
 
val collect_rev_indices_from_node : exp_node -> ID.t * exp_node list 
val collect_rev_indices : exp -> ID.t  * exp_node list   
val collect_indices_from_node : exp_node -> ID.t * exp_node list
val collect_indices : exp -> ID.t * exp_node list 
   

(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
val typed_exp : Type.elt_t -> exp -> exp_node
val bool_exp : exp->exp_node 
val int16_exp : exp->exp_node    
val int_exp : exp -> exp_node   
val f32_exp : exp -> exp_node  
val f64_exp : exp -> exp_node  

(* CAST AN EXPRESSION TO A NEW TYPE 
   (or leave it alone if it's already that type
*)

val cast : Type.elt_t -> exp_node -> exp_node 

val common_type : ?t:ImpType.t -> exp_node list -> ImpType.t  
val typed_op : Prim.scalar_op -> ?t:Type.elt_t -> exp_node list -> exp_node 

(* Same as typed_op, but with comparison operators which always return bools *) 
val cmp_op : Prim.scalar_op -> ?t:Type.elt_t -> exp_node list -> exp_node 

(* CUDA stuff *)
type vec3 = { x: exp_node; y: exp_node; z: exp_node}
val mk_vec3 : (coord -> exp_node) -> vec3 

val threadIdx : vec3 
val blockIdx : vec3 
val blockDim : vec3
val gridDim : vec3 

(* GENERAL IMP EXPRESSIONS *)

val uint32 : Int32.t -> exp_node    
val int32 : Int32.t -> exp_node 

val uint : int -> exp_node
val int : int -> exp_node 

val zero : exp_node 
val one : exp_node 
val infinity : exp_node 
    
val float : float -> exp_node   
val double : float -> exp_node  
val bool : bool -> exp_node 

val select :exp_node -> exp_node ->exp_node -> exp_node 

val idx : exp_node -> exp_node -> exp_node 

val dim : int -> exp_node -> exp_node 
 
    
    
val len : exp_node -> exp_node 
val max_ : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node 
val min_ : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node   

val mul : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node  
val ( *$ ) :  ?t:Type.elt_t -> exp_node -> exp_node -> exp_node

val add : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( +$ ): ?t:Type.elt_t -> exp_node -> exp_node -> exp_node

val div : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( /$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node 

val sub : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( -$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node 

val mod_ : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( %$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node 

val safe_div_ : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node

val lt : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( <$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
 
val lte : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( <=$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node

val gt : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( >$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node 

val gte : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( >=$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node 

val eq : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( =$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node

val neq : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node
val ( <>$ ) : ?t:Type.elt_t -> exp_node -> exp_node -> exp_node


val not_ : exp_node -> exp_node 
val (!$) : exp_node -> exp_node 
 
val and_ : exp_node -> exp_node -> exp_node
val (&&$) : exp_node -> exp_node -> exp_node 
 
val or_ : exp_node -> exp_node -> exp_node
val (||$) : exp_node -> exp_node -> exp_node 

val sqrt32 : exp_node -> exp_node  
val sqrt64 : exp_node -> exp_node   

val ln_32 : exp_node -> exp_node 
val ln_64 : exp_node -> exp_node  

val id_of : exp_node -> ID.t 
val var : ?t:ImpType.t -> ID.t -> exp_node

val max_simplify : exp_node -> exp_node -> exp_node
val mul_simplify : exp_node -> exp_node -> exp_node
val add_simplify : exp_node -> exp_node -> exp_node 

val max_exp_node_list : exp_node list -> exp_node
val prod_exp_node_list : exp_node list -> exp_node
val sum_exp_node_list : exp_node list -> exp_node  

val highest_rank_exp :  exp_node array -> exp_node 