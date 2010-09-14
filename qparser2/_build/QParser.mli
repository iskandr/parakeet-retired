exception Error

type token = 
  | VERB_AMEND of (string)
  | VERB of (string)
  | UNRECOGNIZED_CHAR of (char)
  | UNMATCHED_QUOTE
  | SYM of (string)
  | STR of (string)
  | SHORT of (int)
  | SEMICOLON
  | RPAREN
  | REAL of (float)
  | RCURLY
  | RBRACKET
  | LPAREN
  | LEXER_ERROR
  | LCURLY
  | LBRACKET
  | INT of (Int32.t)
  | IMPLICIT_INT of (Int32.t)
  | IMPLICIT_FLOAT of (float)
  | ID of (string)
  | FLOAT of (float)
  | EOL
  | EOF
  | DOUBLE_COLON
  | CONTROL of (string)
  | COLON
  | BIT_VEC of (int list)
  | BIT of (int)
  | BAD_ID of (string)
  | BAD_ADVERB of (string)
  | BACKSLASH_T
  | ADVERB of (string)


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (QSyntax.syntax_node)
val interp_line: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (QSyntax.syntax_node)