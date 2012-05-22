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

let register_untyped_function 
      ~(name:string) 
      ~(globals:string list) 
      ~(positional_args:string list)
      ~(default_arg_names:string list)
      ~(default_arg_values:AST.node list)
      ~(body: AST.node) =
  Printf.printf "REGISTER COMPACT\n%!";
  Gc.compact();
  Gc.compact(); 
  assert (List.length default_arg_names = List.length default_arg_values); 
  let _ = Analyze_AST.analyze_ast body in
  Printf.printf "-- Creating global env for SSA conversion\n%!"; 
  let ssaEnv = AST_to_SSA.Env.GlobalScope FnManager.get_untyped_id in
  let args = { 
    Args.names = globals@positional_args;
    defaults = List.combine default_arg_names default_arg_values
  }
  in
  Printf.printf "-- Doing SSA conversion\n%!";   
  let fn = AST_to_SSA.translate_fn ~name ssaEnv args body in
  FnManager.add_untyped name fn;
  fn.UntypedSSA.fn_id
  
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

let syntax_value_to_runtime_value (v : UntypedSSA.value_node) : Ptr.t Value.t =
  match v.UntypedSSA.value with 
    | UntypedSSA.Num n -> Value.Scalar n 
    | _ -> failwith "Default args must be a scalar"
   

let run_function 
      ~(untyped_id:FnId.t)
      ~(globals:Ptr.t Value.t list)
      ~(positional_args:Ptr.t Value.t list)
      ~(keyword_names:string list)
      ~(keyword_values : Ptr.t Value.t list) : ret_val =
  Printf.printf "RUN | COMPACT\n%!";
  Gc.compact();
  Gc.compact(); 
  assert (List.length keyword_names = List.length keyword_values); 
  let actuals = { 
    Args.values = globals @ positional_args;
    keywords = List.combine keyword_names keyword_values;
  }
  in  
  (*Timing.clear Timing.runTemplate;
  Timing.clear Timing.typedOpt;
  Timing.start Timing.runTemplate;*)
  
  let untypedFn = FnManager.get_untyped_function untyped_id in
  let formals : UntypedSSA.value_node Args.formal_args = 
    untypedFn.UntypedSSA.inputs 
  in 
  let formals : Ptr.t Value.t Args.formal_args = 
    Args.apply_to_formal_values syntax_value_to_runtime_value formals
  in 
   
  let namedArgVals =  Args.bind formals actuals in
  let idArgVals = 
    List.map 
      (fun (name, v) -> 
        let id = 
          String.Map.find name untypedFn.UntypedSSA.input_names_to_ids
        in
        id, v 
      )
      namedArgVals
  in 
  let idArgEnv = ID.Map.of_list idArgVals in 
 
  (*  Error errorMsg *)
  let actualTypes = Args.apply_to_actual_values Value.type_of actuals in     
  try
    let signature = Signature.from_args actualTypes in
    let typedFundef = get_specialized_function untyped_id signature in
    let reorderedArgs = 
      List.map 
        (fun id -> ID.Map.find id idArgEnv) 
        typedFundef.TypedSSA.input_ids
    in 
    let result = Runtime.call typedFundef reorderedArgs in  
    Success result
  with exn -> begin
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
  end
