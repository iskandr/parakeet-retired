 
type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim   
type coord = X | Y | Z

type array_field = 
  | RangeStart
  | RangeStop
  | ShiftData
  | ShiftAmt
  | ShiftDim
  | ShiftDefault
  | RotData
  | RotDim
  | RotAmt
  | SliceDim
  | SliceStart
  | SliceStop
  | FrozenDim
  | FrozenIdx
  
type value =  
  | Var of ID.t
  | Const of ParNum.t
  | CudaInfo of cuda_info * coord
and value_node = { 
  value : value; 
  value_type : ImpType.t;  
}

type exp =
  | Val of value_node  
  | Op of  Type.elt_t * Prim.scalar_op * value_node list
  | Select of ImpType.t * value_node * value_node * value_node
  | Cast of ImpType.t * value_node
  | Idx of value_node * value_node list
  | DimSize of value_node * value_node 
  | FreezeDim of value_node * value_node * value_node 
  | ArrayField of array_field * value_node
  
and exp_node = {
  exp : exp;
  exp_type : ImpType.t;
}

and stmt = 
  | If of value_node * block * block
  | While of exp_node * block
  | Set of ID.t * exp_node 
  | SetIdx of ID.t * value_node list * exp_node
  | SyncThreads
  | Comment of string
  (*
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE 
  *)
and block = stmt list

   
type storage = 
  | Global
  | Private
  | Shared
  | Alias


type fn = {
  input_ids : ID.t list;
  output_ids : ID.t list; 
  local_ids : ID.t list; 
  
  storage : storage ID.Map.t;
  types : ImpType.t ID.Map.t;
  shapes : SymbolicShape.t ID.Map.t;
  
  body : block;
}


val get_var_type : fn -> ID.t -> ImpType.t 
val get_var_storage : fn -> ID.t -> storage 
val get_var_shape : fn -> ID.t -> SymbolicShape.t

val cuda_info_to_str : cuda_info -> string 
val coord_to_str : coord -> string
 
val val_to_str : value -> string 
val val_node_to_str : value_node -> string
val val_node_list_to_str : value_node list -> string

val exp_to_str : exp -> string
val exp_node_to_str : exp_node -> string 
 
val stmt_to_str : ?spaces:string -> stmt -> string 
val block_to_str : ?spaces:string -> stmt list -> string 
val fn_to_str : fn -> string

val always_const_val : value_node -> bool 
val always_const_exp : exp_node -> bool 

