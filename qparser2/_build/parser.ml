open QSyntax 

let () =
  let stdinbuf = Lexing.from_channel stdin in
   
  while true do
    (* Read line by line. *)
    let linebuf = Lexing.from_string (QLexer.line stdinbuf) in
    try
      (* Run the parser on a single line of input. *)
      let node =  QParser.interp_line QLexer.token linebuf in 
      Printf.printf "==> %s\n" (QSyntax.node_to_str node);
      flush stdout
    with
    (*| Lexer.Error msg ->
    Printf.fprintf stderr "%s%!" msg
    | QParser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
    *)
    _ -> Printf.fprintf stderr "something went awry!"
  done