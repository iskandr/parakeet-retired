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
 
 exception Error of string 
 let err msg = raise (Error msg)     
 
}

let newline = "\n" | "\r"
let digit = ['0'-'9']
let bit = ['0' '1']
let int = ('-')? digit+ 
let simple_float = ('-')? digit+  ('.' (digit+)?)?
let scientific_notation = int 'e' simple_float
let float = simple_float | scientific_notation 

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t' ]+	
  
let basic_binop = "+" | "-" | "*" | "%" | "^" | "=" | "<>"  | "<"|"<="|">"|">=" 
					  | "?"| "~" | "&" | "|" | "," | "@" | "."  | "#" | "$" | "!"  | "_"
            | "|" | "&"

let binop = basic_binop | "ij" | "xprev" | "xbar" | "rotate" | "msum" | "mmin" 
						| "mmax" | "mdev" | "mcount" | "mavg" | "wsum" | "mavg" | "mmu"  
						|  "lsq" | "cross" | "vs" | "sv" |  "like" | "bin"  | "mod" 
						| "xexp" | "xlog" | "inter" | "union" | "except" | "each" 
						| "insert" | "upsert" |  "0:" 
           
let control = "do" | "while" | "if" 
let adverb = "'" | "/" | "\\" | "\\:" |  "/:" | "':" | "\\:/:" | "/:\\:"

(* strip one line from an input stream *) 
rule line = parse
| ([^ '\n']* '\n') as line { line }
| eof { err "Unexpected EOF in line lexer" }
and token = parse
  | "\\t" { BACKSLASH_T }
  | "\\l" whitespace+ ([^ ' ' '\t' '\n' '\r' ]+ as filename)  { LOAD filename } 
  | (basic_binop as str) ':' { BINOP_AMEND str }
  | binop as str { BINOP str }
  
  (* having an adverb overloaded as the comment character is remarkably dumb *)
  | newline { incr_lineno lexbuf;  EOL  }   
  | adverb as str 
  { 
    if str = "/" && lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_curr_p.pos_bol + 1 
    then 
       single_comment lexbuf
    else ADVERB str 
  }
  | newline whitespace* '/' { incr_lineno lexbuf; single_comment lexbuf }
  |  '/'  {  single_comment lexbuf } (*whitespace+*)
  
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
  | (int as num) 'i'  { INT (Int32.of_string num) }
  | (int as num) 'h'  { SHORT (int_of_string num) }
  | (int as num) 'j'  { LONG (Int64.of_string num) } 
  | int as num { int_vec [Int64.of_string num] lexbuf }  
  
  
  | (float as num) 'f'  { FLOAT (float_of_string num ) }
  | (float as  num) 'e'  { REAL (float_of_string num ) }
  | float as num        { float_vec [float_of_string num] lexbuf }
  
  | (bit as ch) 'b'  { BIT (int_of_char ch - (int_of_char '0')) }
  | (bit bit+ as str) 'b' { BIT_VEC (bit_vec str) }
  | id as text { ID text }
  | (digit id) as text 
    { err (sprintf "bad identifier: %s" text) }

  | '\"' ([^ '\"']* as text) '\"' { STR text }
  | '\"' { err "unmatched quote" }
  | '`' { SYM "" } 
  | '`' (id as text) { SYM text }
  | '`' id whitespace '`' id 
    { err "cannot use symbol as a function" }
  | whitespace { token lexbuf }	
  | eof		{  EOF }
  | _ as c { err (sprintf "unrecognize character: %c" c) }  
and single_comment = parse
  | newline  { incr_lineno lexbuf; EOL }
  | eof { EOF }
  | _ { single_comment lexbuf }
(* if we've seen only integers in our vector so far *) 
and int_vec elts  = parse 
  | whitespace { int_vec elts lexbuf }
  | (float as num) 'f' 
    { 
      let elts' = (float_of_string num) :: (List.map Int64.to_float elts) in 
      FLOAT_VEC (List.rev elts')
    } 
  | (float as num) 'e' 
    {
      let elts' = (float_of_string num) :: (List.map Int64.to_float elts) in 
      REAL_VEC (List.rev elts')
    } 
  | (int as num) 'h' 
    {
      let elts' = (int_of_string num) :: (List.map Int64.to_int elts) in 
       SHORT_VEC (List.rev elts')
    }
  | (int as num) 'i' 
    {
      let elts' = (Int32.of_string num) :: (List.map Int64.to_int32 elts) in 
      INT_VEC (List.rev elts')
    }
  | (int as num) 'j' 
    {
      let elts' = (Int64.of_string num) :: elts in 
      LONG_VEC (List.rev elts')
    }
  | int as num 
    { let elts' = (Int64.of_string num) :: elts in int_vec elts' lexbuf } 
  | float as num 
    { let elts' = (float_of_string num) :: (List.map Int64.to_float elts) in 
      float_vec elts' lexbuf
    } 
  | _ 
    { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1; 
      INT_VEC (List.rev (List.map Int64.to_int32 elts)) 
    }    
and float_vec elts = parse
  | whitespace+ { float_vec elts lexbuf }
  | (float as num) 'f' 
    { 
      let elts' = (float_of_string num) :: elts in 
      FLOAT_VEC (List.rev elts') 
    } 
  | (float as num) 'r' 
    { 
      let elts' = (float_of_string num) :: elts in 
      REAL_VEC (List.rev elts') 
    } 
 
  | float as num 
    { let elts' = (float_of_string num) :: elts in 
      float_vec elts' lexbuf 
    } 
  | _ { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1; 
        FLOAT_VEC (List.rev elts) 
      } 
 
{

}
