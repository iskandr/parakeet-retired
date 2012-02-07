(* pp: -parser o pa_macro.cmo *)

open Base
open Printf
open SSA

let init() =
  let gcParams = Gc.get() in
  Gc.set { gcParams with
    Gc.minor_heap_size = 10000;
    space_overhead = 90;
  }
  (*HardwareInfo.hw_init ();*)
  (*Cuda.init ()*)

(* not sure where else to initialize *)
let _ = init ()

let register_untyped_function ~name ~globals ~args astNode =
  IFDEF DEBUG THEN
    Printf.printf "[FrontEnd] Received untyped AST: %s (%s)\n %s\n%!"
      name
      (String.concat ", " args)
      (AST.to_str astNode)
  ENDIF;
  let _ = Analyze_AST.analyze_ast astNode in
  let ssaEnv = AST_to_SSA.Env.GlobalScope FnManager.get_untyped_id in
  let argNames = globals @ args in
  let fn = AST_to_SSA.translate_fn ~name ssaEnv argNames astNode in
  FnManager.add_untyped ~optimize:true name fn;
  Printf.printf "Registered %s as %s (id = %d)\n%!"
    name
    (FnId.to_str fn.SSA.fn_id)
    fn.SSA.fn_id
  ;
  fn.SSA.fn_id

let rec register_untyped_functions = function
  | (name, globals, args, astNode)::rest ->
    let _ = register_untyped_function ~name ~globals ~args astNode in
    register_untyped_functions rest
  | [] -> ()

let print_all_timers () =
  Timing.print_timers();
  let gpuTimes =
    Timing.get_total Timing.gpuTransfer +.
    Timing.get_total Timing.gpuExec +.
    Timing.get_total Timing.ptxCompile +.
    Timing.get_total Timing.gpuMalloc
  in
  Printf.printf "Compiler overhead: %f\n"
    (Timing.get_total Timing.runTemplate -. gpuTimes)
  ;
  Pervasives.flush_all()

type ret_val =
  | Success of Ptr.t Value.t list
  | Error of string
  | Pass

let run_function untypedId ~globals ~args : ret_val =
  IFDEF DEBUG THEN
    printf "[FrontEnd.run_function] Running %s (id=%d)\n%!"
      (FnId.to_str untypedId)
      untypedId
    ;
  ENDIF;
  Timing.clear Timing.runTemplate;
  Timing.clear Timing.typedOpt;
  Timing.clear Timing.ptxCompile;
  Timing.clear Timing.gpuTransfer;
  Timing.clear Timing.gpuExec;
  Timing.clear Timing.gpuMalloc;
  Timing.start Timing.runTemplate;
  let args = globals @ args in
  let argTypes = List.map Value.type_of args in
  let untypedFn = FnManager.get_untyped_function untypedId in
  IFDEF DEBUG THEN
     printf "[FrontEnd.run_function] untyped function body: %s\n%!"
      (SSA.fn_to_str untypedFn);
  ENDIF;
  let nargs = List.length args in
  let arity = List.length untypedFn.input_ids in
  if nargs <> arity then
    failwith $
      Printf.sprintf
        "[Parakeet] arity mismatch-- expected %d, got %d" arity nargs
  else
  let signature = Signature.from_input_types argTypes in
  IFDEF DEBUG THEN
    printf
      "[FrontEnd.run_function] calling specializer for argument types: %s\n"
      (Type.type_list_to_str argTypes);
  ENDIF;
  let fnVal = SSA.GlobalFn untypedId in
  let typedFundef =
    match FnManager.maybe_get_specialization fnVal signature with
    | Some typedId -> FnManager.get_typed_function typedId
    | None ->
      FnManager.optimize_untyped_functions ();
      let unoptimizedTyped = Specialize.specialize_fn_id untypedId signature
      in
      (* now optimize the typed fundef and any typed functions it depends on *)
      FnManager.optimize_typed_functions ();
      FnManager.get_typed_function unoptimizedTyped.SSA.fn_id
  in
  let resultVals = Interp.run typedFundef args in
  print_all_timers();
  Timing.clear Timing.untypedOpt;
  Pervasives.flush_all ();
   (* assume only one result can be returns *)
  Success resultVals
