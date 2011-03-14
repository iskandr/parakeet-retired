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
  let mk = QSyntax.mk_syntax_node
  let ($) f x = f x 
%}


%token <float> FLOAT
%token <float> REAL 
%token <Int32.t> INT  
%token <int> SHORT 
%token <Int64.t> LONG 

%token <float list> FLOAT_VEC
%token <float list> REAL_VEC
%token <Int64.t list> LONG_VEC  
%token <Int32.t list> INT_VEC 
%token <int list> SHORT_VEC

%token <int> BIT 
%token <int list> BIT_VEC

%token <string> ID
%token <string> STR
%token <string> SYM

%token <string> BINOP 
%token <string> ADVERB 
%token <string> BINOP_AMEND
%token <string> CONTROL 
%token EOF 
%token EOL
%token COLON DOUBLE_COLON SEMICOLON
%token LPAREN RPAREN 
%token LBRACKET RBRACKET
%token LCURLY RCURLY 
%token BACKSLASH_T 
%token <string> LOAD  


%right BINOP 

%start program
%start interp_line 

%type <QSyntax.syntax_node> program 
%type <QSyntax.syntax_node> interp_line
 
%%

program: EOL* stmts=program_statements { mk $ BlockExp stmts } 

program_statements:   
| s=stmt sep* EOF { [s] } 
| s=stmt sep+ rest=program_statements { s :: rest }   

interp_line: 
| items=separated_list(SEMICOLON, stmt) EOL 
  { mk $ BlockExp items } 

sep: 
| SEMICOLON {} 
| EOL {}
  
/* statements are any expression or a top-level directive like \l or \t */    
stmt: 
| e = compound_exp { e } 
| BACKSLASH_T e=compound_exp { mk $ TimeExp e }
| filename=LOAD { mk $ LoadExp filename }  

/* expressions or function applications */  
compound_exp: 
| e = simple_exp { e} 
| a = amend { a}
/* implicit function application by juxtaposing two terms */ 
| lhs=simple_exp; rhs=compound_exp { mk $ AppExp(lhs, [rhs]) }
/* binary operator */  
| arg1=simple_exp; op=BINOP; arg2=compound_exp 
  { mk $ AppExp(mk $ Id op, [arg1;arg2 ]) }
/* binary operator + adverb     
| arg1=simple_exp; op=BINOP; adverb=ADVERB; arg2=compound_exp
  { mk $ AppExp(mk $ Id adverb, [mk $ Id op; arg1; arg2]) } 
 
| arg1=simple_exp; name=ID; adverb=ADVERB; arg2=compound_exp
   { mk $ AppExp(mk $ Id adverb, [mk $ Id name; arg1; arg2]) }
*/ 
| control=CONTROL LBRACKET args=args_list RBRACKET 
   { mk $ ControlExp (control, args) }  
| arg1=simple_exp; op=simple_exp; adverb=ADVERB; arg2=compound_exp
   { mk $ AppExp(mk $ Id adverb, [op; arg1; arg2]) }


amend: 
| name=ID; COLON rhs=compound_exp { mk $ DefExp(name, rhs) }
| name=ID; binop=BINOP_AMEND; rhs=compound_exp 
  { mk $ DefExp (name, mk $ AppExp (mk $ Id binop, [mk $ Id name; rhs])) }    

arg: 
 | c = compound_exp { c }
 | b=inline_block { b } 

args_sep: 
 | SEMICOLON EOL* { } 

args_list: 
 | args = separated_list(args_sep, arg) { args } 


simple_exp:
/* tighter binding form of function application */
| lhs=simple_exp; LBRACKET args=args_list RBRACKET { mk $ AppExp(lhs, args) }  
| lhs=ADVERB; LBRACKET args=args_list RBRACKET { mk$ AppExp(mk $ Id lhs, args) } 
/* parens used to specify evaluation order */ 
| LPAREN e = compound_exp RPAREN { e }
/* parens used to create an array */ 
| LPAREN 
    first=compound_exp SEMICOLON 
    rest=separated_nonempty_list(SEMICOLON, compound_exp)
  RPAREN 
  { mk $ ArrExp (first::rest) } 
| n = num_or_vec { n } 
| f = fn { f }  
| id =  ID { mk $ Id id }  
| str = STR { mk $ StrLit str } 
| sym = SYM { mk $ SymLit sym }
| op=BINOP { mk $ Id op }  



/* uniform vector or number */ 
num_or_vec: 
| b = BIT { mk $ BitLit b }  
| bits =  BIT_VEC  { mk $ ArrExp (List.map (fun x -> mk (BitLit x)) bits)  }   
| f = REAL { mk $ RealLit f }  
| f = FLOAT { mk $ FloatLit f }  
| i = INT  { mk $ IntLit i }  
| i = LONG { mk $ LongLit i } 
| i =  SHORT { mk $ ShortLit i } 
| elts = FLOAT_VEC 
  { match List.map (fun f -> mk $ FloatLit f) elts with 
    | [f] -> f
    | fs -> mk $ ArrExp fs
  }
| elts = REAL_VEC
  { match List.map (fun r -> mk $ RealLit r) elts with 
    | [r] -> r
    | rs -> mk $ ArrExp rs
  }   
| elts = INT_VEC 
 { match List.map (fun i -> mk $ IntLit i) elts with 
    | [i] -> i
    | is -> mk $ ArrExp is
  } 
| elts = SHORT_VEC 
 { match List.map (fun s -> mk $ ShortLit s) elts with 
  | [s] -> s 
  | ss -> mk $ ArrExp ss
  }
| elts = LONG_VEC 
 { match List.map (fun s -> mk $ LongLit s) elts with 
    | [s] -> s
    | ss -> mk $ ArrExp ss
  } 
 
 
fn:
| LCURLY option(sep+); body=block; RCURLY { mk $ SimpleLamExp body } 
| LCURLY 
    LBRACKET ids = formals_list RBRACKET;  
    option(sep+) body=block;  
  RCURLY   
  { mk $ LamExp(ids, body) }

formals_list: 
 | names = separated_list(SEMICOLON, ID) { names } 

block: 
| items = separated_nonempty_list(sep+, compound_exp) { mk $ BlockExp items }  

inline_block:
| LBRACKET items = separated_nonempty_list(SEMICOLON, compound_exp) RBRACKET { mk $ BlockExp items }  
