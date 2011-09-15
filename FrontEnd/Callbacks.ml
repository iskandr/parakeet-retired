let _ = Callback.register "shape_create" Shape.create
let _ = Callback.register "sizeof_dyn_type" DynType.sizeof
let _ = Callback.register "shape_rank" Shape.rank
let _ = Callback.register "mk_ast_info" AST_Info.mk_ast_info
let _ = Callback.register "register_untyped_function"
  FrontEnd.register_untyped_function
let _ = Callback.register "run_function" FrontEnd.run_function
let _ = Callback.register "get_prim" ExternalLibNames.get_prim

let _ = Callback.register "type_rank" DynType.nest_depth
let _ = Callback.register "get_elt_type" DynType.elt_type

(* not callbacks, but values used directly by the frontend *) 
let _ = Callback.register "bool_t" DynType.BoolT
let _ = Callback.register "char_t" DynType.CharT 
let _ = Callback.register "int32_t" DynType.Int32T 
let _ = Callback.register "int64_t" DynType.Int64T
let _ = Callback.register "float32_t" DynType.Float32T 
let _ = Callback.register "float64_t" DynType.Float64T





