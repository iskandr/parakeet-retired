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

  assert (List.length default_arg_names = List.length default_arg_values); 
  let args = { 
    Args.names = globals@positional_args;
    defaults = List.combine default_arg_names default_arg_values
  }
  in 
  Printf.printf 
    "[register_untyped_function] Args: %s\n Body:%s\n\n%!"
    (Args.formal_args_to_str ~value_to_str:AST.to_str args)
    (AST.to_str body);
  let _ = Analyze_AST.analyze_ast body in
  Printf.printf "-- Creating global env for SSA conversion\n%!"; 
  let ssaEnv = 
     AST_to_SSA.Env.GlobalScope FnManager.get_untyped_id 
  in
  Printf.printf "-- Doing SSA conversion\n%!";   
  let fn = AST_to_SSA.translate_fn ~name ssaEnv args body in
  Printf.printf "%s\n%!" (UntypedSSA.fn_to_str fn);
  Printf.printf "-- Adding fn to FnManager\n%!";
  FnManager.add_untyped name fn;
  let fnId = fn.UntypedSSA.fn_id in 
  Printf.printf "-- Returning from register_untyped\n%!";
  fnId 
  

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
    | UntypedSSA.NoneVal -> Value.NoneVal 
    | other -> failwith $ Printf.sprintf 
        "Default args must be a scalar, got %s"
        (UntypedSSA.value_to_str other)
   
type value = Ptr.t Value.t 
type values = value list 

let exn_to_str = function 
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
  | exn ->  Printexc.to_string exn

let handle_error exn  =
  let errorMsg = exn_to_str exn in 
  Printf.printf "\nParakeet failed with the following error:\n";
  Printf.printf "- %s\n\n" errorMsg;
  Printf.printf "OCaml Backtrace:\n";
  Printexc.print_backtrace Pervasives.stdout;
  Printf.printf "\n%!";
  Error errorMsg

let infer_axes arrayArgTypes = function 
  | Some axes -> axes
  | None -> 
    AdverbHelpers.const_axes $
     AdverbHelpers.infer_adverb_axes_from_types 
       (Args.all_actual_values arrayArgTypes)

let run_adverb ~adverb ~untyped_id ~globals ~init ~axes ~fixed ~arrays : ret_val = 
  Printf.printf "[Parakeet] In run_adverb\n%!"; 
  (* fixed keywords not yet supported *) 
  assert (fixed.Args.keywords = []);
  (* array keywords not yet supported *) 
  assert (arrays.Args.keywords = []); 
  let arrayTypes : Type.t Args.actual_args = 
    Args.apply_to_actual_values Value.type_of arrays
  in
  let axes : int list = infer_axes arrayTypes axes in 
  let fixedTypes : Type.t Args.actual_args = 
    Args.apply_to_actual_values Value.type_of fixed 
  in 
  let initTypes : Type.t list = 
    List.map Value.type_of init 
  in 
  let globalTypes : Type.t list = 
    List.map Value.type_of globals 
  in
  let arrayEltTypes =
    Args.apply_to_actual_values (Type.peel ~num_axes:(List.length axes)) arrayTypes
  in  
  let combinedTypes : Type.t Args.actual_args = 
    Args.prepend_actual_values globalTypes 
      (Args.combine_actual_args fixedTypes 
        (Args.prepend_actual_values initTypes arrayEltTypes))
  in
  let signature = 
    Signature.from_args combinedTypes 
  in  
  try
    let typedFn = 
      get_specialized_function untyped_id signature 
    in
    let adverbInfo : (TypedSSA.fn, Ptr.t Value.t list, int list) Adverb.info = { 
      Adverb.adverb = adverb; 
      adverb_fn = typedFn; 
      fixed_args = fixed.Args.values; 
      init = if init <> [] then Some init else None; 
      axes = axes;
      array_args = arrays.Args.values; 
    } 
    in 
    Success (Runtime.adverb adverbInfo)
  with exn -> handle_error exn   
  
let run_function 
      ~(untyped_id:FnId.t)
      ~(globals:Ptr.t Value.t list)
      ~(actuals:Ptr.t Value.t Args.actual_args) : ret_val =
  Printf.printf "Compact\n%!"; 
  (*Gc.compact();  *)
  Printf.printf "[run_function] In OCaml, prepending %d globals\n%!"
    (List.length globals); 
  let actuals = Args.prepend_actual_values globals actuals in 
  Printf.printf "[run_function] Getting untyped function\n%!"; 
  let untypedFn = FnManager.get_untyped_function untyped_id in
  let formals : UntypedSSA.value_node Args.formal_args = 
    untypedFn.UntypedSSA.inputs 
  in 
  
  let formals : Ptr.t Value.t Args.formal_args = 
    Args.apply_to_formal_values syntax_value_to_runtime_value formals
  in 
   
  (* map from names to values *)
  Printf.printf "[run_function] Binding args...\n%!"; 
  let boundArgs =  Args.bind formals actuals in
  Printf.printf "[FrontEnd] Bound args: %s\n%!" 
    (String.concat ", " (List.map fst boundArgs));
  let actualTypes = Args.apply_to_actual_values Value.type_of actuals in
  let signature = Signature.from_args actualTypes in
  try
    let typedFundef = get_specialized_function untyped_id signature in
    let result = Runtime.call typedFundef (List.map snd boundArgs) in  
    Success result
  with exn -> handle_error exn 
