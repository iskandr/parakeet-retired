(* pp: -parser o pa_macro.cmo *)

open Base
open Printf

let init() =
  let gcParams = Gc.get() in
  Gc.set { gcParams with
    Gc.minor_heap_size = 10000;
    space_overhead = 85;
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
  FnManager.add_untyped name fn;
  let fnId = fn.UntypedSSA.fn_id in
  IFDEF DEBUG THEN
    Printf.printf "Registered %s as %s (id = %d)\n Body: %s\n%!"
      name
      (FnId.to_str fnId)
      fnId
      (UntypedSSA.fn_to_str fn)
    ;
  ENDIF;
  fnId

let rec register_untyped_functions = function
  | (name, globals, args, astNode)::rest ->
    let _ = register_untyped_function ~name ~globals ~args astNode in
    register_untyped_functions rest
  | [] -> ()

let print_all_timers () =
  (*
  Timing.print_timers();
  Printf.printf "Compiler overhead: %f\n" (Timing.get_total Timing.runTemplate);
  Pervasives.flush_all()*)
  ()

type ret_val =
  | Success of Ptr.t Value.t list
  | Error of string
  | Pass

let get_specialized_function untypedId signature =
  let fnVal = UntypedSSA.GlobalFn untypedId in
  match FnManager.maybe_get_specialization fnVal signature with
  | Some typedId ->
    FnManager.get_typed_function typedId
  | None ->
    let unoptimizedTyped = Specialize.specialize_fn_id untypedId signature in
    (* now optimize the typed fundef and any typed functions it depends on *)
    FnManager.optimize_typed_functions ();
    FnManager.get_typed_function unoptimizedTyped.TypedSSA.fn_id

let run_function untypedId ~globals ~args : ret_val =
  IFDEF DEBUG THEN
    printf "[FrontEnd.run_function] Running %s (id=%d) with args = %s\n%!"
      (FnId.to_str untypedId)
      untypedId
      (Value.list_to_str args)
    ;
  ENDIF;
  (*Timing.clear Timing.runTemplate;
  Timing.clear Timing.typedOpt;
  Timing.start Timing.runTemplate;*)
  let args = globals @ args in
  let argTypes = List.map Value.type_of args in
  let untypedFn = FnManager.get_untyped_function untypedId in
  let nargs = List.length args in
  let arity = List.length untypedFn.UntypedSSA.input_ids in
  if nargs <> arity then
    let errorMsg =
      Printf.sprintf
        "[Parakeet] arity mismatch-- expected %d, got %d"
        arity
        nargs
    in
    Error errorMsg
  else begin
    let signature = Signature.from_input_types argTypes in
    IFDEF DEBUG THEN
      printf
        "[FrontEnd.run_function] calling specializer for argument types: %s\n"
        (Type.type_list_to_str argTypes);
    ENDIF;
    let result =
      try
        let typedFundef = get_specialized_function untypedId signature in
        let outputs = Runtime.call typedFundef args in
        Success outputs
      with exn -> (
        let errorMsg =
          match exn with
          | TypeAnalysis.TypeError(txt, srcOpt) ->
            let srcStr =
              Option.map_default
                (fun srcInfo -> "at " ^ (SrcInfo.to_str srcInfo))
                "(no source info)"
                srcOpt
            in
            Printf.sprintf "Type Error: %s %s" txt srcStr
          | ShapeInference.ShapeInferenceFailure txt ->
            Printf.sprintf "Shape Error: %s" txt
          | _ ->  Printexc.to_string exn
        in
        Printf.printf "\nParakeet failed with the following error:\n";
        Printf.printf "- %s\n\n" errorMsg;
        Printf.printf "OCaml Backtrace:\n";
        Printexc.print_backtrace Pervasives.stdout;
        Printf.printf "\n%!";
        Error errorMsg
      )
    in
    (*print_all_timers();
    Timing.clear Timing.untypedOpt;*)
    result
  end
