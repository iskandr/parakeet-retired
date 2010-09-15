exception Error

type token = 
  | SYM of (string)
  | STR of (string)
  | SHORT_VEC of (int list)
  | SHORT of (int)
  | SEMICOLON
  | RPAREN
  | REAL_VEC of (float list)
  | REAL of (float)
  | RCURLY
  | RBRACKET
  | LPAREN
  | LONG_VEC of (Int64.t list)
  | LONG of (Int64.t)
  | LOAD of (string)
  | LCURLY
  | LBRACKET
  | INT_VEC of (Int32.t list)
  | INT of (Int32.t)
  | ID of (string)
  | FLOAT_VEC of (float list)
  | FLOAT of (float)
  | EOL
  | EOF
  | DOUBLE_COLON
  | CONTROL of (string)
  | COLON
  | BIT_VEC of (int list)
  | BIT of (int)
  | BINOP_AMEND of (string)
  | BINOP of (string)
  | BACKSLASH_T
  | ADVERB of (string)


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (QSyntax.syntax_node)
val interp_line: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (QSyntax.syntax_node)