open QSyntax 
open Printf 
let rec repl () = 
    (* Read line by line. *)
    let line = read_line () in
    if line = "\\\\" then () else 
     
      let linebuf = Lexing.from_string (line ^ "\n") in
      begin try
        (* Run the parser on a single line of input. *)
        let node =  QParser.interp_line QLexer.token linebuf in 
        Printf.printf "==> %s\n" (QSyntax.node_to_str node);
        flush stdout
      with
      | QParser.Error ->
         Printf.fprintf stdout "Syntax error at column %d.\n%!" 
        (Lexing.lexeme_start linebuf + 1)
      
      | QLexer.Error msg -> Printf.fprintf stdout "[lexer_error] %s\n" msg
      end
      ;
      repl ()  
    

let () =
  if Array.length Sys.argv <= 1 then repl () 
  else 
    let filename = Sys.argv.(1) in 
    let lexbuf = 
      try 
        Lexing.from_channel (open_in filename) 
       with 
        | _ -> failwith (sprintf "error opening file %s" filename)
    in 
    try 
      let node =  QParser.program QLexer.token lexbuf in 
      Printf.printf "==> %s\n" (QSyntax.node_to_str node)
    with 
      | QParser.Error ->
         let pos = Lexing.lexeme_start_p lexbuf in  
         Printf.fprintf stdout "At line %d, column %d: syntax error.\n%!" 
         pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      
      | QLexer.Error msg -> Printf.fprintf stdout "[lexer_error] %s\n" msg
     