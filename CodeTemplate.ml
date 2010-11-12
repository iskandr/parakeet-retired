open Base

open HostVal
open Printf
open InterpState 
open SourceInfo
(* need the TypeCheck module included to not break the release build...
   we really need a more resilient build script for CQInterface
*)
open TypeCheck 
open SSA 
open QStdLib 

let _ = Printf.printf "any: %s\n" (SSA.fundef_to_str QStdLib.any) 

let _ = Printexc.record_backtrace true 

let checkpoint p =
  Gc.compact ();
  prerr_endline ("checkpoint at position " ^ p)

let c_compact () =
  prerr_endline "In c_compact";
  Gc.compact ()

type globals = string list
type locals = string list
type fn_body_str = string
type fn_name = string

type run_template_ret_val = Success of host_val | Pass | Error of string

type module_entry = fn_name * locals * globals * fn_body_str

let build_function_body_ast body =
    let lexbuf = Lexing.from_string body in  
    let bodySyntax = 
      try
         QParser.program QLexer.token lexbuf 
      with 
      | QParser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in  
        Printf.fprintf stdout "Parser error at line %d, column %d.\n" 
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        ; exit 1
     | QLexer.Error msg -> 
        let pos = Lexing.lexeme_start_p lexbuf in  
        Printf.fprintf stdout "Lexer error at line %d, column %d: %s\n" 
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1) msg
        ; exit 1
    in  
    try 
      QSyntax_to_AST.syntax_to_ast bodySyntax
	  with
    | StaticError txt -> failwith txt
    | SourcedError (txt, debug) ->
        failwith
          (sprintf "%s on line %d at position %d" txt debug.line debug.col)

(* assume functions have been sorted so that a function def appears
   before it's used 
*) 
let rec extend_interp_state interpState = function 
  | [] -> interpState  
  | (name,locals,globals,bodyText)::rest ->
      let bodyAST = build_function_body_ast bodyText in
      (* global function lookup function used by AST_to_SSA conversion *)
      let ssaEnv = 
        AST_to_SSA.Env.GlobalScope (InterpState.get_untyped_id interpState)  
      in
      let argNames = locals @ globals in  
      let fundef = AST_to_SSA.translate_fn ssaEnv argNames bodyAST in  
      InterpState.add_untyped interpState ~opt_queue:true name fundef; 
      extend_interp_state interpState rest 
   
(* replace the concept of a "module" with just an untyped interpState 
   which also preserves the original string names of its functions
*)   
let gen_module_template entries =
(* should 'state' be a returned value? it's actually just an 
   imperative modification of QStdLib.initState 
*)
  let interpState = extend_interp_state QStdLib.initState entries in
  (* run optimizations on accumulated queue of user-defined functions *) 
  InterpState.optimize_untyped_functions interpState; 
  interpState  
     
  
let get_function_template interpState name =
  let untypedId = InterpState.get_untyped_id interpState name in 
  (untypedId, interpState) 
  
let run_template 
    (untypedId, interpState) 
    (globals : HostVal.host_val list)  
    (locals : HostVal.host_val list) =
  IFDEF DEBUG THEN printf "entered run_template... \n"; ENDIF; 
  (* TODO: For now, make these calls here. *)
  HardwareInfo.hw_init ();
  LibPQ.init ();
  (* TODO: Make the timings more controllable (mem transfer e.g.) *)
  let startTime =   Unix.gettimeofday () in
  let args = globals @ locals in
  let argTypes = List.map HostVal.get_type args in
  let untypedFn = InterpState.get_untyped_function interpState untypedId in 
  IFDEF DEBUG THEN 
     printf "[run_template] untyped function body: %s\n" 
      (SSA.fundef_to_str untypedFn);
  ENDIF;  
  let nargs = List.length args in
  let arity = List.length (untypedFn.input_ids) in 
  if nargs <> arity then failwith
    (sprintf "[run_template] arity mismatch-- expected %d, got %d" arity nargs)
  else
  let signature = Signature.from_input_types argTypes in
  IFDEF DEBUG THEN 
    printf
      "[run_template] calling specializer for argument types: %s \n"
      (DynType.type_list_to_str argTypes);
  ENDIF;
  (* ignore the returned fundef because it's unoptimized *)  
  let unoptimized = 
    Specialize.specialize_function_id interpState untypedId signature 
  in
  InterpState.optimize_typed_functions interpState; 
  let typedFundef = 
    InterpState.get_typed_function interpState unoptimized.fn_id 
  in 
  IFDEF DEBUG THEN 
    printf "[run_template] calling evaluator on specialized code: \n";
    printf "%s\n" (SSA.fundef_to_str typedFundef);
  ENDIF;  
  let fnTable = InterpState.get_typed_function_table interpState in 
  let resultVals = Eval.eval fnTable typedFundef args in  
  let result = Success (List.hd resultVals)  in     
  printf "Total Time: %f\n" (Unix.gettimeofday () -. startTime);
  IFDEF DEBUG THEN flush stdout; ENDIF; 
  result
