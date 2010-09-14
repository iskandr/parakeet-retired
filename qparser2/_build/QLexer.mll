{
  open Printf
  open Lexing
  
  include QParser
  
  let curr_col lexbuf = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol
  
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

    (* push the position forward some lines and reset offset from start of line *)  
    let incr_pos lines offset lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + lines;
      pos_bol = pos.pos_cnum - offset;
    }
      
 let bit_vec str =
      let n = String.length str in 
      let rec builder i acc = 
          if i < 0 then acc
          else 
            let num = (int_of_char str.[i]) - ( int_of_char '0') in 
            builder (i-1) (num::acc)
      in builder (n-1) []        
}

let newline = "\n" | "\r"
let digit = ['0'-'9']
let bit = ['0' '1']
let int = ('-')? digit+ 
let float = ('-')? digit+  ('.' (digit+)?)?
let scientific_notation = int 'e' float
  
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t' ]+	
  
let basic_verb = "+" | "-" | "*" | "%" | "^" | "=" | "<>"  | "<"|"<="|">"|">=" 
								| "?"| "~" | "&" | "|" | "," | "@" | "."  | "#" | "$" | "!"  
								| "_"
								
let verb =  basic_verb | "ij" | "xprev" | "xbar" | "rotate" | "msum" | "mmin" 
						| "mmax" | "mdev" | "mcount" | "mavg" | "wsum" | "mavg" | "mmu"  
						|  "lsq" | "cross" | "vs" | "sv" |  "like" | "bin"  | "mod" 
						| "xexp" | "xlog" | "inter" | "union" | "except" | "each" 
						| "insert" | "upsert" | "til" | "0:"
          
let control = "do" | "while" | "if" 
let adverb = "'" | "/" | "\\" | "\\:" |  "/:" | "':" | "\\:/:" | "/:\\:"

(* strip one line from an input stream *) 
rule line = parse
| ([^ '\n'] * '\n') as line { line }
| eof { failwith "Unexpected EOF in line lexer" }
and token = parse
  | "\\t" { BACKSLASH_T }
  | (basic_verb as str) ':' { VERB_AMEND str }
  | verb as str { VERB str }
  (* having an adverb overloaded as the comment character is remarkably dumb *)
  | whitespace  (adverb as str) 
    { 
       if str = "/" then ( single_comment lexbuf) else ADVERB str 
    }
  | adverb as str { 
                    (* this weird case is here only if a / occurs *)
                    (* at the very start of the line *) 
                    if  lexbuf.lex_curr_p.pos_cnum = 1 && str ="/"then
                      single_comment lexbuf 
                    else ADVERB str 
                  }
  | control as str { CONTROL str }
  | ":" {  COLON }
  | "::" {DOUBLE_COLON}
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | (int as num) 'i' { INT (Int32.of_string num) }
  | (int as num) 'h'  { SHORT (int_of_string num) }
  | (int as num) { IMPLICIT_INT (Int32.of_string num) }
  | (scientific_notation as num) 'f'  { FLOAT (float_of_string num) }
  | (scientific_notation as num) 'e' { REAL  (float_of_string num) }
  | (scientific_notation as num)     { IMPLICIT_FLOAT (float_of_string num) }
  | (float as num) 'f'  { FLOAT (float_of_string num ) }
  |  float as num  { IMPLICIT_FLOAT (float_of_string num ) }
  | (float as  num) 'e'  { REAL (float_of_string num ) }
  | (bit as ch) 'b'  { BIT (int_of_char ch - (int_of_char '0')) }
  | (bit bit+ as str) 'b' { BIT_VEC (bit_vec str) }
  | id as text { ID text }
  | (digit id) as text { BAD_ID text }
  | newline {incr_lineno lexbuf;  EOL  }  
  | '\"' ([^ '\"']* as text) '\"' { STR text }
  | '\"' { UNMATCHED_QUOTE}
  | '`' { SYM "" } 
  | '`' (id as text) { SYM text }
  | '`' id whitespace '`' id { LEXER_ERROR }
  | whitespace { token lexbuf }	
  | eof		{ EOF }
  | _ as c {  UNRECOGNIZED_CHAR c }  
and single_comment = parse
  | newline  {EOL }
  | eof { EOF  }
  | _ { single_comment lexbuf }

{

}
