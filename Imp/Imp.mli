 
type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim   
type coord = X | Y | Z

type exp = 
  | Var of ID.t
	| Const of ParNum.t
	| Cast of Type.elt_t * exp_node
  | Op of Type.elt_t * Prim.scalar_op * exp_node list 
  | Select of ImpType.t * exp_node * exp_node * exp_node 
  | Idx of exp_node * exp_node list    
  | DimSize of exp_node * exp_node 
  | CudaInfo of cuda_info * coord 
and exp_node = { 
  exp : exp; 
  exp_type : ImpType.t;  
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
  input_types : ImpType.t array;
          
  output_ids : ID.t array; 
  output_id_set : ID.t MutableSet.t; 
  output_types : ImpType.t array;
  
  local_id_set : ID.t MutableSet.t; 
  
  types : (ID.t, ImpType.t) Hashtbl.t; 
  sizes:  (ID.t, exp_node list) Hashtbl.t; 
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


