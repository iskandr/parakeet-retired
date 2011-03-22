open Base
open SSA 
open Printf

let init() =
  let gcParams = Gc.get() in
  Gc.set { gcParams with Gc.minor_heap_size = 128000; space_overhead = 90 };
  HardwareInfo.hw_init ();
  Cuda.init ()
  
(* not sure where else to initialize *) 
let _ = init () 
  
(* global interpreter state *) 
let interpState = StdLib.initState 

let register_untyped_function ~name ~globals ~args astNode =
  (* FIX: for now, we're reusing the Analyze_AST module used by 
     the Q preprocessor, 
     Problems: 
        - it uses linear-time PSet instead of log-time String.Set
        - it's slow and poorly implemented
        - it wastes a lot of time computing sets of volatile 
          variables which aren't relevant outside the Q preprocessor
        
     It does, however, populate the AST info fields with info about 
     uses and defs later used by AST_to_SSA
    *) 
  let _ = Analyze_AST.analyze_ast astNode in  
  let ssaEnv = 
    AST_to_SSA.Env.GlobalScope (InterpState.get_untyped_id interpState)  
  in
  let argNames = globals @ args in  
  let fundef = AST_to_SSA.translate_fn ssaEnv argNames astNode in  
  InterpState.add_untyped interpState ~optimize:true name fundef; 
  fundef.SSA.fn_id 
  
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
    Timing.get_total Timing.ptxCompile
  in
  Printf.printf "Compiler overhead: %f\n"
    (Timing.get_total Timing.runTemplate -. gpuTimes)
  ;
  Pervasives.flush_all()

type ret_val = Success of HostVal.host_val | Pass | Error of string

let run_function untypedId ~globals ~args =
  Timing.clear_all();
  Timing.start Timing.runTemplate; 
  let args = globals @ args in
  let argTypes = List.map HostVal.get_type args in
  let untypedFn = InterpState.get_untyped_function interpState untypedId in
  IFDEF DEBUG THEN
     printf "[run_function] untyped function body: %s\n"
      (SSA.fundef_to_str untypedFn);
  ENDIF;
  let nargs = List.length args in
  let arity = List.length untypedFn.input_ids in
  if nargs <> arity then 
    failwith $
      Printf.sprintf 
        "[Parakeet] arity mismatch-- expected %d, got %d" 
        arity 
        nargs
  else
  let signature = Signature.from_input_types argTypes in
  IFDEF DEBUG THEN 
    printf
      "[run_function] calling specializer for argument types: %s\n"
      (DynType.type_list_to_str argTypes);      
  ENDIF;
  
  let typedFundef = 
    match 
      InterpState.maybe_get_specialization 
        interpState 
        (SSA.GlobalFn untypedId)
        signature 
    with
    | Some id -> InterpState.get_typed_function interpState id  
    | None ->
      (* 
         first optimize any untyped functions which haven't yet been optimized
       *)
      InterpState.optimize_untyped_functions interpState;   
      let unoptimizedTyped = 
        Specialize.specialize_function_id interpState untypedId signature
      in
      (* now optimize the typed fundef and any typed functions it depends on *)
      InterpState.optimize_typed_functions interpState;   
      InterpState.get_typed_function interpState unoptimizedTyped.SSA.fn_id
  in  
  IFDEF DEBUG THEN
    printf "[run_function] calling evaluator on specialized code: \n";
    printf "%s\n" (SSA.fundef_to_str typedFundef);
  ENDIF;
  let fnTable = InterpState.get_typed_function_table interpState in
  let resultVals = Eval.eval fnTable typedFundef args in
  print_all_timers();
  Pervasives.flush_all (); 
   (* assume only one result can be returns *) 
  Success (List.hd resultVals) 
