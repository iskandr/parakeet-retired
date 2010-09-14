/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/


%{
  open QSyntax
  let mk = mk_syntax_node
  let ($) f x = f x 
  
%}


%token <float> FLOAT
%token <float> IMPLICIT_FLOAT 
%token <float> REAL 
%token <Int32.t> IMPLICIT_INT 
%token <Int32.t> INT 
%token <int> SHORT 
%token <int> BIT 
%token <int list> BIT_VEC
%token <string> ID
%token <string> STR
%token <string> SYM
%token <string> VERB 
%token <string> ADVERB 
%token <string> VERB_AMEND
%token <string> CONTROL 
%token EOF 
%token EOL
%token LEXER_ERROR 
%token <string> BAD_ADVERB
%token <string> BAD_ID 
%token UNMATCHED_QUOTE
%token <char> UNRECOGNIZED_CHAR 
%token COLON DOUBLE_COLON SEMICOLON
%token LPAREN RPAREN 
%token LBRACKET RBRACKET
%token LCURLY RCURLY 
%token BACKSLASH_T  

%start program
%start interp_line 

%type <QSyntax.syntax_node> program 
%type <QSyntax.syntax_node> interp_line
 
%%

program:   
| items=separated_list(sep+, stmt) EOF { mk $ BlockExp items }

interp_line: 
| items=separated_list(SEMICOLON, stmt) EOL { mk $ BlockExp items} 

sep: 
| SEMICOLON {} 
| EOL {}
  
/* statements are any expression or a top-level directive like \l or \t */    
stmt: 
| e = expr0 { e } 

expr0: 
| e = expr1 { e} 
| e = app { e }

expr1:
| LPAREN e = expr0 RPAREN { e }
| e = lit { e }
| f = fn { f }  


app: 
| lhs = expr1 rhs = expr0 { mk $ AppExp(lhs, [rhs]) }
| lhs = expr1 LBRACKET args = args_list RBRACKET 
  { mk $ AppExp(lhs, args) } 


fn:
| LCURLY body = block RCURLY { mk $ SimpleLamExp body } 
| LCURLY 
    LBRACKET ids = formals_list RBRACKET  
    option(sep+) body = block  
  RCURLY   
  { mk $ LamExp(ids, body) }

formals_list: 
 | names = separated_list(SEMICOLON, ID) { names } 

block: 
| items = separated_nonempty_list(sep+, expr0) { mk $ BlockExp items }  

args_list: 
 | args = separated_list(SEMICOLON, expr0) { args } 

lit: 
| f = FLOAT { mk $ FloatLit f } 
| f = IMPLICIT_FLOAT { mk $ FloatLit f }  
| f = REAL { mk $ RealLit f }  
| i = IMPLICIT_INT { mk $ IntLit i }  
| i = INT { mk $ IntLit i }  
| i =  SHORT { mk $ ShortLit i }      
| b = BIT { mk $ BitLit b }  
/*| bits =  BIT_VEC  {  } */  
| id =  ID { mk $ Id id }  
| str = STR { mk $ StrLit str } 
| sym = SYM { mk $ SymLit sym } 
    