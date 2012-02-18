open SymbolicShape

type env = SymbolicShape.t ID.Map.t
exception ShapeInferenceFailure of string

(* NORMALIZE_SHAPE: Rewrite shape expression so it only refers to input
   shapes and not any intermediate shape variables.

   inputs:
     - set of input ids
     - raw map id -> shape
     - cached canonical shape map
     - shape to be canonicalized
   outputs:
     - canonicalized shape
     - new canonicalized shape env
*)
val normalize_shape : ID.Set.t -> env -> env -> SymbolicShape.t -> SymbolicShape.t * env
val infer_shape_env :  FnTable.t -> SSA.fn -> env
val infer_normalized_shape_env : FnTable.t -> SSA.fn -> env
val infer_normalized_output_shapes : FnTable.t -> SSA.fn -> SymbolicShape.t list
val infer_call_result_shapes :
      FnTable.t -> SSA.fn -> SymbolicShape.t list -> SymbolicShape.t list

val typed_fn_is_shapely : SSA.fn -> bool