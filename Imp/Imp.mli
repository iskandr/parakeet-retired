 
type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim   
type coord = X | Y | Z

type exp = 
  | Var of ID.t
	| Const of ParNum.t
  | Idx of exp_node * exp_node list
  | Op of  Type.elt_t * Prim.scalar_op * exp_node list
  | Select of ImpType.t * exp_node * exp_node * exp_node
  | Cast of ImpType.t * exp_node
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
   
type array_storage = 
  | Global
  | Private
  | Shared
  | Slice

type symbolic_shape = exp_node list 


type var_info = { 
	var_type : ImpType.t; 
	array_storage : array_storage option;
	symbolic_shape : symbolic_shape;  
} 

type fn = {
	input_ids : ID.t list;
  output_ids : ID.t list; 
  local_ids : ID.t list;  
  var_info : var_info ID.Map.t; 
	body : block;
}

val get_var_type : fn -> ID.t -> ImpType.t 
val get_var_storage : fn -> ID.t -> array_storage 
val get_var_shape : fn -> ID.t -> symbolic_shape 


val cuda_info_to_str : cuda_info -> string 
val coord_to_str : coord -> string 
val exp_node_to_str : exp_node -> string 
val exp_to_str : exp -> string   
val exp_node_list_to_str : exp_node list -> string 
val stmt_to_str : ?spaces:string -> stmt -> string 
val block_to_str : ?spaces:string -> stmt list -> string 
val fn_to_str : fn -> string

val always_const : exp_node -> bool 


