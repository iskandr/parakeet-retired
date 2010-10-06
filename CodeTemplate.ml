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

type module_template = {
  fn_name_to_id : (string, ID.t) PMap.t; 
  program : Program.program; 
}

let build_function_ast name locals globals body =

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
      let bodyAST = QSyntax_to_AST.syntax_to_ast bodySyntax in
      let lam = AST.mk_lam_node (globals @ locals) bodyAST in
      AST.mk_def_node name lam 
	  with
    | StaticError txt -> failwith txt
    | SourcedError (txt, debug) ->
        failwith
          (sprintf "%s on line %d at position %d" txt debug.line debug.col)

let  build_program_ast entries =
  let rec aux revDefs = function
  | [] -> AST.mk_block_node (List.rev revDefs)
  | (name,locals,globals,bodyText)::rest ->
      debug (Printf.sprintf "!!! [build_program] making ast: %s\n" name); 
      let fnDef = build_function_ast name locals globals bodyText in
      aux (fnDef::revDefs) rest
  in aux [] entries

let gen_module_template entries =
  debug "==(Q to AST)==> \n %!";
  let ast = build_program_ast entries in
  debug (sprintf "%s \n%!"  (AST.node_to_str ast));
  
  debug "==(AST to Untyped SSA)==> \n %!";
  let code, idEnv = AST_to_SSA.translate_stmt PMap.empty ast in
  debug (sprintf "%s \n%!"  (SSA.block_to_str code)); 
  
  debug "==(Creating Optimized Program)==> \n %!";
  let program = Program.create_from_untyped_block code in 
  { fn_name_to_id = idEnv; program = program } 
 
let get_function_template moduleTemplate name =
  let untypedId = PMap.find name moduleTemplate.fn_name_to_id in
  let prog = moduleTemplate.program in  
  (untypedId, prog) 
  
let run_template (untypedId,program) globals locals =
  debug "entered run_template... \n";
  let startTime =   Unix.gettimeofday () in
  let args = globals @ locals in
  let argTypes = List.map (fun v -> v.host_t) args in
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
  let results = 
    Eval_SSA.eval 
        program.cuda_code_cache 
        program.typed_functions 
        typedFundef 
        args 
  in 
  (debug (sprintf "Total Time: %f\n" (Unix.gettimeofday () -. startTime));
   flush stdout;
   (* assume Q expects only single result value *)  
   List.hd results
  ) 
 
