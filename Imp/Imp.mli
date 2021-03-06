type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim
type coord = X | Y | Z

type array_field =
  | ArrayData
  | ArrayShape
  | ArrayStrides
  | PtrData
  | PtrLen
  | RangeStart
  | RangeStop
  | ShiftData
  | ShiftAmt
  | ShiftDim
  | ShiftDefault
  | RotData
  | RotDim
  | RotAmt

val fields_of_type : ImpType.t -> array_field list
val field_types : ImpType.t -> ImpType.t list
val array_field_pos : array_field -> int

type value =
  | Var of ID.t
  | Const of ParNum.t
  | VecConst of ParNum.t list
  | CudaInfo of cuda_info * coord
  | Idx of value_node * value_node list
  | VecSlice of value_node * value_node * int
  | Op of ImpType.t * Prim.scalar_op * value_node list
  | Select of ImpType.t * value_node * value_node * value_node
  | Cast of ImpType.t * value_node
  | DimSize of value_node * value_node
  | FixDim of value_node * value_node * value_node
  | Slice of value_node * value_node * value_node * value_node
  | ArrayField of array_field * value_node

and value_node = {
  value : value ;
  value_type : ImpType.t;
}

type value_nodes = value_node list

val recursively_apply :
  ?delay_level:int -> (value_node -> value_node) -> value_node -> value_node

type stmt =
  | If of value_node * block * block
  | While of value_node * block (* test, body *)
  | Set of value_node * value_node
  | SyncThreads
  | Comment of string
and block = stmt list

val recursively_apply_to_stmt :
  lhs:(value_node -> value_node) -> rhs:(value_node -> value_node) ->
    stmt -> stmt

type storage =
  | Global
  | Local
  | Alias
  | CudaShared

type fn = {
  id : FnId.t;
  input_ids : ID.t list;
  output_ids : ID.t list;
  local_ids : ID.t list;

  storage : storage ID.Map.t;
  types : ImpType.t ID.Map.t;
  shapes : SymbolicShape.t ID.Map.t;

  body : block;
}

val empty_fn : fn

val input_types : fn -> ImpType.t list
val output_types : fn -> ImpType.t list
val local_types : fn -> ImpType.t list

val output_shapes : fn -> SymbolicShape.t list
val local_shapes : fn -> SymbolicShape.t list

val get_var_type : fn -> ID.t -> ImpType.t
val get_var_storage : fn -> ID.t -> storage
val get_var_shape : fn -> ID.t -> SymbolicShape.t

val array_field_to_str : array_field -> string
val cuda_info_to_str : cuda_info -> string
val coord_to_str : coord -> string
val array_storage_to_str : storage -> string
val value_type : value_node -> ImpType.t
val value_types : value_node list -> ImpType.t list

val value_to_str : value -> string
val value_node_to_str : value_node -> string
val value_nodes_to_str : value_node list -> string

val stmt_to_str : ?spaces:string -> stmt -> string
val block_to_str : ?spaces:string -> stmt list -> string
val fn_to_str : fn -> string

val always_const : value_node -> bool

