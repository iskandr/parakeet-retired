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
  IFDEF DEBUG THEN
    Printf.printf "Registered %s as %s (id = %d)\n Body: %s\n%!"
      name
      (FnId.to_str fn.SSA.fn_id)
      fn.SSA.fn_id
      (SSA.fn_to_str fn)
    ;
  ENDIF;
  fn.SSA.fn_id

let rec register_untyped_functions = function
  | (name, globals, args, astNode)::rest ->
    let _ = register_untyped_function ~name ~globals ~args astNode in
    register_untyped_functions rest
  | [] -> ()

let print_all_timers () =
  Timing.print_timers();
  Printf.printf "Compiler overhead: %f\n" (Timing.get_total Timing.runTemplate);
  Pervasives.flush_all()

type ret_val =
  | Success of Ptr.t Value.t list
  | Error of string
  | Pass

let get_specialized_function untypedId signature =
  let fnVal = SSA.GlobalFn untypedId in
  match FnManager.maybe_get_specialization fnVal signature with
  | Some typedId ->
    FnManager.get_typed_function typedId
  | None ->
    FnManager.optimize_untyped_functions ();
    let unoptimizedTyped = Specialize.specialize_fn_id untypedId signature in
    (* now optimize the typed fundef and any typed functions it depends on *)
    FnManager.optimize_typed_functions ();
    FnManager.get_typed_function unoptimizedTyped.SSA.fn_id

let run_function untypedId ~globals ~args : ret_val =
  IFDEF DEBUG THEN
    printf "[FrontEnd.run_function] Running %s (id=%d)\n%!"
      (FnId.to_str untypedId)
      untypedId
    ;
  ENDIF;
  Timing.clear Timing.runTemplate;
  Timing.clear Timing.typedOpt;
  Timing.start Timing.runTemplate;
  let args = globals @ args in
  let argTypes = List.map Value.type_of args in
  let untypedFn = FnManager.get_untyped_function untypedId in
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


  let result =
    try
      let typedFundef = get_specialized_function untypedId signature in
      Success (Interp.run typedFundef args)
    with exn ->
      begin
        let errorMsg = Printexc.to_string exn in
        Printf.printf "\nParakeet execution failed with the following error:\n";
        Printf.printf "- %s\n\n" errorMsg;
        Printf.printf "OCaml Backtrace:\n";
        Printexc.print_backtrace Pervasives.stdout;
        Printf.printf "\n%!";
        Error errorMsg
      end
  in
  (*print_all_timers();*)
  Timing.clear Timing.untypedOpt;
  result
