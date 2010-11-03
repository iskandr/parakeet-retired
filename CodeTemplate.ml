open Base

open HostVal
open Printf
open Program
open SourceInfo
open SSA 

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
let rec build_untyped_program
     (fnNames : SSA.FnId.t String.Map.t) 
     (fundefs : SSA.fundef FnId.Map.t) = function 
  | [] -> Program.create_untyped fnNames fundefs  
  
  | (name,locals,globals,bodyText)::rest ->
      let bodyAST = build_function_body_ast bodyText in
      let env = AST_to_SSA.Env.GlobalScope fnNames in
      let argNames = locals @ globals in  
      let fundef = AST_to_SSA.translate_fn env argNames bodyAST in  
      let fnId = fundef.SSA.fn_id in   
      let fnNames' = String.Map.add name fnId fnNames in
      let fundefs' = FnId.Map.add fnId fundef fundefs in  
      build_untyped_program fnNames' fundefs' rest 
   
(* replace the concept of a "module" with just an untyped program 
   which also preserves the original string names of its functions
*)   
let gen_module_template entries =
   build_untyped_program String.Map.empty FnId.Map.empty entries 
  
let get_function_template program name =
  let untypedId = 
    Hashtbl.find program.Program.name_to_untyped_id name
  in
  (untypedId, program) 
  
let run_template 
    (untypedId, program) 
    (globals : HostVal.host_val list)  
    (locals : HostVal.host_val list) =
  debug "entered run_template... \n";
  let startTime =   Unix.gettimeofday () in
  let args = globals @ locals in
  let argTypes = List.map HostVal.get_type args in
  let untypedFn = FnTable.find untypedId program.untyped_functions in
  debug 
    (sprintf 
      "[run_template] untyped function body: %s\n" 
      (SSA.fundef_to_str untypedFn)); 
  let nargs = List.length args in
  let arity = List.length (untypedFn.input_ids) in 
  if nargs <> arity then failwith
    (sprintf "[run_template] arity mismatch-- expected %d, got %d" arity nargs)
  else
  let signature = Signature.from_input_types argTypes in
  debug (sprintf
    "[run_template] calling specialzer for argument types: %s \n"
    (DynType.type_list_to_str argTypes));  
  let typedFundef = 
    Specialize.specialize_function_id program untypedId signature 
  in
  debug "[run_template] calling evaluator on specialized code: \n";
  debug (sprintf "%s\n" (SSA.fundef_to_str typedFundef)); 
  let resultVals = Eval.eval program.typed_functions typedFundef args in  
  let result = Success (List.hd resultVals)  in     
  debug (sprintf "Total Time: %f\n" (Unix.gettimeofday () -. startTime));
  flush stdout;  
  result
