let _ = Callback.register "ktypenum_to_ocaml_type" QType.ktypenum_to_ocaml_type
let _ = Callback.register "dyn_type_to_ktypenum" QType.dyn_type_to_ktypenum
let _ = Callback.register "gen_module_template" QFrontEnd.gen_module_template
let _ = 
  Callback.register "get_function_template" QFrontEnd.get_function_template
let _ = Callback.register "run_template" QFrontEnd.run_template
let _ = Callback.register "c_compact" QFrontEnd.c_compact

