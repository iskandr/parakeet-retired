
exception ShapeInferenceFailure of string

val infer_shape_env :  FnTable.t -> SSA.fundef -> SymbolicShape.shape ID.Map.t
val infer_output_shapes : FnTable.t -> SSA.fundef -> SymbolicShape.shape list    