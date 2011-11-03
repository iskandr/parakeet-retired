let _ = Callback.register "shape_create" Shape.create
let _ = Callback.register "sizeof_dyn_type" Type.sizeof
let _ = Callback.register "shape_rank" Shape.rank
let _ = Callback.register "mk_ast_info" AST.mk_ast_info
let _ = Callback.register "register_untyped_function"
  FrontEnd.register_untyped_function
let _ = Callback.register "run_function" FrontEnd.run_function
let _ = Callback.register "get_prim" ExternalLibNames.get_prim

let _ = Callback.register "type_rank" Type.rank
let _ = Callback.register "get_elt_type" Type.elt_type

(* not callbacks, but values used directly by the frontend *) 
let _ = Callback.register "bool_t" Type.BoolT
let _ = Callback.register "char_t" Type.CharT 
let _ = Callback.register "int32_t" Type.Int32T 
let _ = Callback.register "int64_t" Type.Int64T
let _ = Callback.register "float32_t" Type.Float32T 
let _ = Callback.register "float64_t" Type.Float64T





