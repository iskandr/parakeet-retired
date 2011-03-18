type coord = X | Y | Z
 
type exp = 
  | Var of ID.t
  | Idx of exp_node * exp_node  
  | Op of Prim.scalar_op * DynType.t * exp_node list 
  | Select of DynType.t * exp_node * exp_node * exp_node 
  | Const of PQNum.num 
  | Cast of DynType.t * exp_node  
  | DimSize of int * exp_node 
  | ThreadIdx of coord 
  | BlockIdx of coord 
  | BlockDim of coord 
  | GridDim of coord
and exp_node = { 
  exp : exp; 
  exp_type : DynType.t;  
} 
and stmt = 
  | If of exp_node * block * block
  | While of exp_node * block
  | Set of ID.t * exp_node 
  | SetIdx of ID.t * exp_node list * exp_node
  | SyncThreads
  | Comment of string
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE 
and block = stmt list
   
and array_storage = 
  | Global
  | Private
  | Shared
  | Slice
and fn = {
  input_ids : ID.t array;
  input_id_set : ID.t MutableSet.t; 
  input_types : DynType.t array;
          
  output_ids : ID.t array; 
  output_id_set : ID.t MutableSet.t; 
  output_types : DynType.t array;
  
  local_id_set : ID.t MutableSet.t; 
  
  types : (ID.t, DynType.t) Hashtbl.t; 
  sizes: (ID.t, exp_node list) Hashtbl.t; 
  array_storage : (ID.t, array_storage) Hashtbl.t;
  
  body : block;
}

val exp_node_to_str : exp_node -> string 
val exp_to_str : exp -> string   
val exp_node_list_to_str : exp_node list -> string 
val stmt_to_str : ?spaces:string -> stmt -> string 
val block_to_str : ?spaces:string -> stmt list -> string 
val fn_to_str : fn -> string

val always_const : exp_node -> bool 

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
val typed_exp : DynType.t -> exp -> exp_node
val bool_exp : exp->exp_node 
val int16_exp : exp->exp_node    
val uint16_exp : exp->exp_node
val int_exp : exp -> exp_node   
val uint_exp : exp -> exp_node  
val f32_exp : exp -> exp_node  
val f64_exp : exp -> exp_node  

(* CAST AN EXPRESSION TO A NEW TYPE 
   (or leave it alone if it's already that type
*)

val cast : DynType.t -> exp_node -> exp_node 

val common_type : ?t:DynType.t -> exp_node list -> DynType.t  
val typed_op : Prim.scalar_op -> ?t:DynType.t -> exp_node list -> exp_node 

(* Same as typed_op, but with comparison operators which always return bools *) 
val cmp_op : Prim.scalar_op -> ?t:DynType.t -> exp_node list -> exp_node 

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
val max_ : ?t:DynType.t -> exp_node -> exp_node -> exp_node 
val min_ : ?t:DynType.t -> exp_node -> exp_node -> exp_node   

val mul : ?t:DynType.t -> exp_node -> exp_node -> exp_node  
val ( *$ ) :  ?t:DynType.t -> exp_node -> exp_node -> exp_node

val add : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( +$ ): ?t:DynType.t -> exp_node -> exp_node -> exp_node

val div : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( /$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node 

val sub : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( -$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node 

val mod_ : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( %$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node 

val safe_div_ : ?t:DynType.t -> exp_node -> exp_node -> exp_node

val lt : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( <$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node
 
val lte : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( <=$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node

val gt : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( >$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node 

val gte : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( >=$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node 

val eq : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( =$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node

val neq : ?t:DynType.t -> exp_node -> exp_node -> exp_node
val ( <>$ ) : ?t:DynType.t -> exp_node -> exp_node -> exp_node


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
val var : ?t:DynType.t -> ID.t -> exp_node

val max_simplify : exp_node -> exp_node -> exp_node
val mul_simplify : exp_node -> exp_node -> exp_node
val add_simplify : exp_node -> exp_node -> exp_node 

val max_exp_node_list : exp_node list -> exp_node
val prod_exp_node_list : exp_node list -> exp_node
val sum_exp_node_list : exp_node list -> exp_node  