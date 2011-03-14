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

(* replace the concept of a "module" with just an untyped interpState 
   which also preserves the original string names of its functions
*)   
(* assume functions have been sorted so that a function def appears
   before it's used 
*) 
let rec gen_module_template = function 
  | [] -> FrontEnd.interpState 
  | (name,globals,locals,bodyText)::rest ->
      IFDEF DEBUG THEN 
        Printf.printf "Building AST for %s\n" name; 
      ENDIF; 
      let bodyAST = build_function_body_ast bodyText in
      IFDEF DEBUG THEN 
        Printf.printf "%s\n" (AST.node_to_str bodyAST);
      ENDIF; 
      (* global function lookup function used by AST_to_SSA conversion *)
      let _= 
        FrontEnd.register_untyped_function ~name ~globals ~args:locals bodyAST 
      in
      gen_module_template rest 
  
let get_function_template interpState name =
  let untypedId = InterpState.get_untyped_id FrontEnd.interpState name in 
  (untypedId, FrontEnd.interpState)  
  
let run_template 
      (untypedId, _)
      (globals : HostVal.host_val list)
      (locals : HostVal.host_val list) =
  IFDEF DEBUG THEN printf "entered run_template... \n"; ENDIF; 
  (* TODO: For now, make these calls here. *)
  FrontEnd.run_function untypedId globals locals 