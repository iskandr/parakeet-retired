(* pp: -parser o pa_macro.cmo *)

(* TODO: Get rid of GPU/Host distinction and replace with arbitrary number *)
(* of backends/memspaces *)

(* the cost model function expect arguments to be described by triplets  *)
(* of their type, shape, and a boolean indicating whether that argument  *)
(* is on the gpu.                                                        *)

type value = DataId.t Value.t
type values = value list

let machine_model = MachineModel.build_machine_model

let value_to_host v = DataManager.to_memspace HostMemspace.id v

let call (fn : SSA.fn) (args:values) =
  IFDEF DEBUG THEN
    Printf.printf "[Scheduler] Calling %s with args %s\n"
      (FnId.to_str fn.SSA.fn_id)
      (String.concat ", " (List.map Value.to_str args));
  ENDIF;
  let results = LLVM_Backend.call fn (List.map value_to_host args) in
  List.map DataManager.from_memspace results

let map ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) (args:values) =
  let fixed' = List.map value_to_host fixed in
  let args' = List.map value_to_host args in
  let results = LLVM_Backend.map ~axes ~fn ~fixed:fixed' args' in
  List.map DataManager.from_memspace results

let reduce ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) ?init (args:values)
  = assert false

let scan ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) ?init (args:values)
  = assert false

let all_pairs ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) (x:value) (y:value)
  = assert false

let array_op (op : Prim.array_op) (args : value list) = assert false
