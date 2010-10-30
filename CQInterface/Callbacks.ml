
(* Main interface functions *)
let _ = Callback.register "gen_module_template" CodeTemplate.gen_module_template
let _ = Callback.register "get_function_template" CodeTemplate.get_function_template
let _ = Callback.register "run_template" CodeTemplate.run_template

(* Support functions *)
let _ = Callback.register "ktypenum_to_ocaml_type" QType.ktypenum_to_ocaml_type
let _ = Callback.register "dyn_type_to_ktypenum" QType.dyn_type_to_ktypenum
