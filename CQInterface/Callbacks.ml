(* Place to collect all of the callbacks from C to OCaml so as to facilitate
   easier builds. *)

let _ = Callback.register "shape_create" Shape.create
let _ = Callback.register "shape_rank" Shape.rank 
let _ = Callback.register "sizeof_dyn_type" DynType.sizeof
let _ = Callback.register "mk_ast_info" AST_Info.mk_ast_info



let _ = Callback.register "ktypenum_to_ocaml_type" QType.ktypenum_to_ocaml_type
let _ = Callback.register "dyn_type_to_ktypenum" QType.dyn_type_to_ktypenum
let _ = Callback.register "gen_module_template" QFrontEnd.gen_module_template
let _ = 
  Callback.register "get_function_template" QFrontEnd.get_function_template
let _ = Callback.register "run_template" QFrontEnd.run_template
let _ = Callback.register "c_compact" QFrontEnd.c_compact

