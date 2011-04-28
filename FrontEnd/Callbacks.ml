let _ = Callback.register "shape_create" Shape.create
let _ = Callback.register "shape_rank" Shape.rank
let _ = Callback.register "sizeof_dyn_type" DynType.sizeof
let _ = Callback.register "mk_ast_info" AST_Info.mk_ast_info
let _ = Callback.register "register_untyped_function"
  FrontEnd.register_untyped_function
let _ = Callback.register "run_function" FrontEnd.run_function
let _ = Callback.register "get_prim" ExternalLibNames.get_prim