exception Error

type token = 
  | SYM of (
# 40 "QParser.mly"
       (string)
# 8 "QParser.ml"
)
  | STR of (
# 39 "QParser.mly"
       (string)
# 13 "QParser.ml"
)
  | SHORT_VEC of (
# 33 "QParser.mly"
       (int list)
# 18 "QParser.ml"
)
  | SHORT of (
# 26 "QParser.mly"
       (int)
# 23 "QParser.ml"
)
  | SEMICOLON
  | RPAREN
  | REAL_VEC of (
# 30 "QParser.mly"
       (float list)
# 30 "QParser.ml"
)
  | REAL of (
# 24 "QParser.mly"
       (float)
# 35 "QParser.ml"
)
  | RCURLY
  | RBRACKET
  | LPAREN
  | LONG_VEC of (
# 31 "QParser.mly"
       (Int64.t list)
# 43 "QParser.ml"
)
  | LONG of (
# 27 "QParser.mly"
       (Int64.t)
# 48 "QParser.ml"
)
  | LOAD of (
# 53 "QParser.mly"
       (string)
# 53 "QParser.ml"
)
  | LCURLY
  | LBRACKET
  | INT_VEC of (
# 32 "QParser.mly"
       (Int32.t list)
# 60 "QParser.ml"
)
  | INT of (
# 25 "QParser.mly"
       (Int32.t)
# 65 "QParser.ml"
)
  | ID of (
# 38 "QParser.mly"
       (string)
# 70 "QParser.ml"
)
  | FLOAT_VEC of (
# 29 "QParser.mly"
       (float list)
# 75 "QParser.ml"
)
  | FLOAT of (
# 23 "QParser.mly"
       (float)
# 80 "QParser.ml"
)
  | EOL
  | EOF
  | DOUBLE_COLON
  | CONTROL of (
# 45 "QParser.mly"
       (string)
# 88 "QParser.ml"
)
  | COLON
  | BIT_VEC of (
# 36 "QParser.mly"
       (int list)
# 94 "QParser.ml"
)
  | BIT of (
# 35 "QParser.mly"
       (int)
# 99 "QParser.ml"
)
  | BINOP_AMEND of (
# 44 "QParser.mly"
       (string)
# 104 "QParser.ml"
)
  | BINOP of (
# 42 "QParser.mly"
       (string)
# 109 "QParser.ml"
)
  | BACKSLASH_T
  | ADVERB of (
# 43 "QParser.mly"
       (string)
# 115 "QParser.ml"
)

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState92
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState86
  | MenhirState79
  | MenhirState76
  | MenhirState72
  | MenhirState68
  | MenhirState64
  | MenhirState59
  | MenhirState58
  | MenhirState55
  | MenhirState50
  | MenhirState49
  | MenhirState44
  | MenhirState41
  | MenhirState40
  | MenhirState34
  | MenhirState33
  | MenhirState27
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState7
  | MenhirState0


# 16 "QParser.mly"
  
  open QSyntax
  let mk = QSyntax.mk_syntax_node
  let ($) f x = f x 

# 165 "QParser.ml"
let _eRR =
  Error

let rec _menhir_goto_program_statements : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_program_statements -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_program_statements) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (rest : 'tv_program_statements) = _v in
        ((let ((_menhir_stack, _menhir_s, s), _, _) = _menhir_stack in
        let _v : 'tv_program_statements = 
# 70 "QParser.mly"
                                      ( s :: rest )
# 185 "QParser.ml"
         in
        _menhir_goto_program_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_program_statements) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (stmts : 'tv_program_statements) = _v in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : (
# 61 "QParser.mly"
      (QSyntax.syntax_node)
# 201 "QParser.ml"
        ) = 
# 66 "QParser.mly"
                                       ( mk $ BlockExp stmts )
# 205 "QParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 61 "QParser.mly"
      (QSyntax.syntax_node)
# 213 "QParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv447) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 61 "QParser.mly"
      (QSyntax.syntax_node)
# 221 "QParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_1 : (
# 61 "QParser.mly"
      (QSyntax.syntax_node)
# 229 "QParser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv446)) : 'freshtv448)) : 'freshtv450)) : 'freshtv452)) : 'freshtv454)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_sep_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_sep_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState90 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv427 * _menhir_state * 'tv_sep) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv425 * _menhir_state * 'tv_sep) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_sep_ = 
# 126 "standard.mly"
    ( x :: xs )
# 248 "QParser.ml"
         in
        _menhir_goto_nonempty_list_sep_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)) : 'freshtv428)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv431 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv429 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_nonempty_list_sep_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv430)) : 'freshtv432)
    | MenhirState10 | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_option_nonempty_list_sep__ = 
# 31 "standard.mly"
    ( Some x )
# 309 "QParser.ml"
         in
        _menhir_goto_option_nonempty_list_sep__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)) : 'freshtv436)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv437 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_nonempty_list_sep_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BACKSLASH_T ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LOAD _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv438)) : 'freshtv440)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fn : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fn -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_fn) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (f : 'tv_fn) = _v in
    ((let _v : 'tv_simple_exp = 
# 123 "QParser.mly"
         ( f )
# 381 "QParser.ml"
     in
    _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)

and _menhir_goto_list_sep_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_sep_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv409 * _menhir_state * 'tv_sep) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * _menhir_state * 'tv_sep) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_sep_ = 
# 116 "standard.mly"
    ( x :: xs )
# 398 "QParser.ml"
         in
        _menhir_goto_list_sep_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)) : 'freshtv410)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_sep_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv411 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, s), _, _) = _menhir_stack in
            let _v : 'tv_program_statements = 
# 69 "QParser.mly"
                  ( [s] )
# 419 "QParser.ml"
             in
            _menhir_goto_program_statements _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)) : 'freshtv414)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)) : 'freshtv420)
    | _ ->
        _menhir_fail ()

and _menhir_reduce26 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_sep -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_nonempty_list_sep_ = 
# 124 "standard.mly"
    ( [ x ] )
# 438 "QParser.ml"
     in
    _menhir_goto_nonempty_list_sep_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_compound_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_compound_exp_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, first), _, rest) = _menhir_stack in
            let _v : 'tv_simple_exp = 
# 121 "QParser.mly"
  ( mk $ ArrExp (first::rest) )
# 465 "QParser.ml"
             in
            _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv405 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv403 * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_separated_nonempty_list_SEMICOLON_compound_exp_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_compound_exp_ = 
# 146 "standard.mly"
    ( x :: xs )
# 484 "QParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_compound_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)) : 'freshtv406)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_nonempty_list_sep__compound_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState64 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state * 'tv_option_nonempty_list_sep__) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (items : 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_) = _v in
        ((let _v : 'tv_block = 
# 178 "QParser.mly"
                                                      ( mk $ BlockExp items )
# 505 "QParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_block) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState23 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv371 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv369 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | RCURLY ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv365 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv363 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (((((_menhir_stack, _menhir_s), _), _, ids), _, _), _, body) = _menhir_stack in
                let _v : 'tv_fn = 
# 172 "QParser.mly"
  ( mk $ LamExp(ids, body) )
# 532 "QParser.ml"
                 in
                _menhir_goto_fn _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv367 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)
        | MenhirState64 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv379 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | RCURLY ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv375 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv373 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), _, _), _, body) = _menhir_stack in
                let _v : 'tv_fn = 
# 167 "QParser.mly"
                                          ( mk $ SimpleLamExp body )
# 561 "QParser.ml"
                 in
                _menhir_goto_fn _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)) : 'freshtv376)
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv377 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)) : 'freshtv382)
        | _ ->
            _menhir_fail ()) : 'freshtv384)) : 'freshtv386)) : 'freshtv388)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv391 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv389 * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_) = _v in
        ((let ((_menhir_stack, _menhir_s, x), _, _) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_ = 
# 146 "standard.mly"
    ( x :: xs )
# 586 "QParser.ml"
         in
        _menhir_goto_separated_nonempty_list_nonempty_list_sep__compound_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)) : 'freshtv392)
    | _ ->
        _menhir_fail ()

and _menhir_goto_amend : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_amend -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_amend) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (a : 'tv_amend) = _v in
    ((let _v : 'tv_compound_exp = 
# 89 "QParser.mly"
            ( a)
# 605 "QParser.ml"
     in
    _menhir_goto_compound_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)) : 'freshtv362)

and _menhir_goto_separated_nonempty_list_args_sep_compound_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_args_sep_compound_exp_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv353 * _menhir_state * 'tv_simple_exp) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_args_sep_compound_exp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_args_sep_compound_exp_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_args_sep_compound_exp__ = 
# 59 "standard.mly"
    ( x )
# 624 "QParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_args_sep_compound_exp__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv357 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) * 'tv_args_sep) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_args_sep_compound_exp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * 'tv_compound_exp) * 'tv_args_sep) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_args_sep_compound_exp_) = _v in
        ((let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_args_sep_compound_exp_ = 
# 146 "standard.mly"
    ( x :: xs )
# 640 "QParser.ml"
         in
        _menhir_goto_separated_nonempty_list_args_sep_compound_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)
    | _ ->
        _menhir_fail ()

and _menhir_reduce18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_sep_ = 
# 114 "standard.mly"
    ( [] )
# 651 "QParser.ml"
     in
    _menhir_goto_list_sep_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_stmt_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_SEMICOLON_stmt_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_stmt_ = 
# 146 "standard.mly"
    ( x :: xs )
# 671 "QParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv344)) : 'freshtv346)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_stmt_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_SEMICOLON_stmt_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__ = 
# 59 "standard.mly"
    ( x )
# 686 "QParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_stmt__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)) : 'freshtv350)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_nonempty_list_sep__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_nonempty_list_sep__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv337 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv335 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv336)) : 'freshtv338)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv340)) : 'freshtv342)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_SEMICOLON_ID_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv329 * _menhir_state) * _menhir_state) * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 805 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 813 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_SEMICOLON_ID_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_ID_ = 
# 146 "standard.mly"
    ( x :: xs )
# 821 "QParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv333 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_SEMICOLON_ID_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_SEMICOLON_ID_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_ID__ = 
# 59 "standard.mly"
    ( x )
# 836 "QParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)) : 'freshtv334)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sep : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sep -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 | MenhirState58 | MenhirState21 | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv319 * _menhir_state * 'tv_sep) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | SEMICOLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv320)) : 'freshtv322)
    | MenhirState90 | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state * 'tv_sep) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMICOLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | BACKSLASH_T | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LOAD _ | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv324)) : 'freshtv326)
    | _ ->
        _menhir_fail ()

and _menhir_goto_compound_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_compound_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState44 | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_compound_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | EOL ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
                _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv248)) : 'freshtv250)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_args_sep_compound_exp_ = 
# 144 "standard.mly"
    ( [ x ] )
# 925 "QParser.ml"
             in
            _menhir_goto_separated_nonempty_list_args_sep_compound_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)) : 'freshtv258)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv261 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 940 "QParser.ml"
        )) * _menhir_state * (
# 43 "QParser.mly"
       (string)
# 944 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv259 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 950 "QParser.ml"
        )) * _menhir_state * (
# 43 "QParser.mly"
       (string)
# 954 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, arg1), _, op), _, adverb), _, arg2) = _menhir_stack in
        let _v : 'tv_compound_exp = 
# 97 "QParser.mly"
  ( mk $ AppExp(mk $ Id adverb, [mk $ Id op; arg1; arg2]) )
# 960 "QParser.ml"
         in
        _menhir_goto_compound_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 968 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 974 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, arg1), _, op), _, arg2) = _menhir_stack in
        let _v : 'tv_compound_exp = 
# 94 "QParser.mly"
  ( mk $ AppExp(mk $ Id op, [arg1;arg2 ]) )
# 980 "QParser.ml"
         in
        _menhir_goto_compound_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)) : 'freshtv266)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * _menhir_state * 'tv_simple_exp) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_simple_exp) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
        let _v : 'tv_compound_exp = 
# 91 "QParser.mly"
                                   ( mk $ AppExp(lhs, [rhs]) )
# 992 "QParser.ml"
         in
        _menhir_goto_compound_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)) : 'freshtv270)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv273 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1000 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv271 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1006 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, name), _, rhs) = _menhir_stack in
        let _v : 'tv_amend = 
# 101 "QParser.mly"
                                  ( mk $ DefExp(name, rhs) )
# 1012 "QParser.ml"
         in
        _menhir_goto_amend _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)) : 'freshtv274)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1020 "QParser.ml"
        )) * (
# 44 "QParser.mly"
       (string)
# 1024 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1030 "QParser.ml"
        )) * (
# 44 "QParser.mly"
       (string)
# 1034 "QParser.ml"
        )) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, name), binop), _, rhs) = _menhir_stack in
        let _v : 'tv_amend = 
# 103 "QParser.mly"
  ( mk $ DefExp (name, mk $ AppExp (mk $ Id binop, [mk $ Id name; rhs])) )
# 1040 "QParser.ml"
         in
        _menhir_goto_amend _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)) : 'freshtv278)
    | MenhirState64 | MenhirState59 | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_compound_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SEMICOLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | RCURLY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_nonempty_list_sep__compound_exp_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1063 "QParser.ml"
             in
            _menhir_goto_separated_nonempty_list_nonempty_list_sep__compound_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv282)) : 'freshtv284)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_compound_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : 'tv_simple_exp = 
# 115 "QParser.mly"
                                 ( e )
# 1089 "QParser.ml"
             in
            _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv291 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv289 * _menhir_state) * _menhir_state * 'tv_compound_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BINOP _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | BIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | BIT_VEC _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | FLOAT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | FLOAT_VEC _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | ID _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | INT_VEC _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LONG _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LONG_VEC _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | REAL _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | REAL_VEC _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | SHORT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | SHORT_VEC _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | SYM _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv290)) : 'freshtv292)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)) : 'freshtv298)
    | MenhirState72 | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv299 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BINOP _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | BIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | BIT_VEC _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | FLOAT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | FLOAT_VEC _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | ID _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | INT_VEC _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LONG _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | LONG_VEC _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | REAL _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | REAL_VEC _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | SHORT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | SHORT_VEC _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | SYM _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv300)) : 'freshtv302)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_compound_exp_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1211 "QParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_compound_exp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)) : 'freshtv310)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
        let _v : 'tv_stmt = 
# 83 "QParser.mly"
                             ( mk $ TimeExp e )
# 1230 "QParser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)
    | MenhirState88 | MenhirState92 | MenhirState0 | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : 'tv_stmt = 
# 82 "QParser.mly"
                   ( e )
# 1242 "QParser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_args_sep_compound_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_args_sep_compound_exp__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv245 * _menhir_state * 'tv_simple_exp) * _menhir_state) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_args_sep_compound_exp__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (xs0 : 'tv_loption_separated_nonempty_list_args_sep_compound_exp__) = _v in
    ((let _v : 'tv_args_list = let args =
      let xs = xs0 in
      
# 135 "standard.mly"
    ( xs )
# 1263 "QParser.ml"
      
    in
    
# 106 "QParser.mly"
                                                 ( args )
# 1269 "QParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv241) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_args_list) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_args_list) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_args_list) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_args_list) = Obj.magic _menhir_stack in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_args_list) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, lhs), _), _, args) = _menhir_stack in
        let _v : 'tv_simple_exp = 
# 113 "QParser.mly"
                                                   ( mk $ AppExp(lhs, args) )
# 1294 "QParser.ml"
         in
        _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv232)) : 'freshtv234)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_args_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState79 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_stmt) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_stmt) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BACKSLASH_T ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | BINOP _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | BIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | BIT_VEC _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | FLOAT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | FLOAT_VEC _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | ID _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | INT_VEC _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | LOAD _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LONG _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LONG_VEC _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | REAL _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | REAL_VEC _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | SHORT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | SHORT_VEC _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | SYM _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv216)) : 'freshtv218)
        | EOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_SEMICOLON_stmt_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1382 "QParser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)
    | MenhirState92 | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_stmt) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMICOLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EOF ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv228)) : 'freshtv230)
    | _ ->
        _menhir_fail ()

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_nonempty_list_sep__ = 
# 29 "standard.mly"
    ( None )
# 1419 "QParser.ml"
     in
    _menhir_goto_option_nonempty_list_sep__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_sep = 
# 77 "QParser.mly"
            ()
# 1432 "QParser.ml"
     in
    _menhir_goto_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_ID__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv211 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_loption_separated_nonempty_list_SEMICOLON_ID__) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (xs0 : 'tv_loption_separated_nonempty_list_SEMICOLON_ID__) = _v in
    ((let _v : 'tv_formals_list = let names =
      let xs = xs0 in
      
# 135 "standard.mly"
    ( xs )
# 1451 "QParser.ml"
      
    in
    
# 175 "QParser.mly"
                                         ( names )
# 1457 "QParser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv207) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_formals_list) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv205 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv203 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | SEMICOLON ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv198)) : 'freshtv200)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 38 "QParser.mly"
       (string)
# 1501 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv195 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1510 "QParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv189 * _menhir_state) * _menhir_state) * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1519 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * _menhir_state) * _menhir_state) * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1526 "QParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv188)) : 'freshtv190)
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1541 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_ID_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1547 "QParser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_ID_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 1557 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_sep = 
# 78 "QParser.mly"
      ()
# 1571 "QParser.ml"
     in
    _menhir_goto_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)

and _menhir_goto_simple_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_simple_exp) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_simple_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BINOP _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_simple_exp) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState33 in
        let (_v : (
# 42 "QParser.mly"
       (string)
# 1593 "QParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 1601 "QParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ADVERB _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 1610 "QParser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState49 in
            let (_v : (
# 43 "QParser.mly"
       (string)
# 1616 "QParser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 1624 "QParser.ml"
            )) * _menhir_state * (
# 43 "QParser.mly"
       (string)
# 1628 "QParser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BINOP _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | BIT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | BIT_VEC _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | FLOAT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | FLOAT_VEC _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | ID _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | INT_VEC _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LCURLY ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LONG _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LONG_VEC _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | REAL _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | REAL_VEC _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | SHORT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | SHORT_VEC _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | SYM _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv166)) : 'freshtv168)
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | EOF | EOL | LBRACKET | RBRACKET | RCURLY | RPAREN | SEMICOLON ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv170)) : 'freshtv172)
    | BIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | BIT_VEC _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | FLOAT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | FLOAT_VEC _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | ID _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INT_VEC _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_simple_exp) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState33 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_simple_exp) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState34 in
            ((let _v : 'tv_loption_separated_nonempty_list_args_sep_compound_exp__ = 
# 57 "standard.mly"
    ( [] )
# 1781 "QParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_args_sep_compound_exp__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv176)) : 'freshtv178)
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LONG _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LONG_VEC _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | REAL _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | REAL_VEC _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | SHORT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | SHORT_VEC _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | EOF | EOL | RBRACKET | RCURLY | RPAREN | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_simple_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : 'tv_compound_exp = 
# 88 "QParser.mly"
                 ( e)
# 1815 "QParser.ml"
         in
        _menhir_goto_compound_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv182)) : 'freshtv184)

and _menhir_goto_num_or_vec : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_num_or_vec -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_num_or_vec) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (n : 'tv_num_or_vec) = _v in
    ((let _v : 'tv_simple_exp = 
# 122 "QParser.mly"
                 ( n )
# 1836 "QParser.ml"
     in
    _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)

and _menhir_reduce65 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 1843 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, op) = _menhir_stack in
    let _v : 'tv_simple_exp = 
# 127 "QParser.mly"
           ( mk $ Id op )
# 1850 "QParser.ml"
     in
    _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_EOL_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_EOL_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state) * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state) * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, xs) = _menhir_stack in
        let x = () in
        let _v : 'tv_list_EOL_ = 
# 116 "standard.mly"
    ( x :: xs )
# 1868 "QParser.ml"
         in
        _menhir_goto_list_EOL_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv144)) : 'freshtv146)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv155 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153) * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _) = _menhir_stack in
        let _v : 'tv_args_sep = 
# 109 "QParser.mly"
                  ( )
# 1880 "QParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151) = _menhir_stack in
        let (_v : 'tv_args_sep) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv149 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) * 'tv_args_sep) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv147 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) * 'tv_args_sep) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)) : 'freshtv154)) : 'freshtv156)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_list_EOL_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BACKSLASH_T ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LOAD _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv158)) : 'freshtv160)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_stmt__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, xs0) = _menhir_stack in
        let _v : (
# 62 "QParser.mly"
      (QSyntax.syntax_node)
# 2022 "QParser.ml"
        ) = let items =
          let xs = xs0 in
          
# 135 "standard.mly"
    ( xs )
# 2028 "QParser.ml"
          
        in
        
# 74 "QParser.mly"
  ( mk $ BlockExp items )
# 2034 "QParser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 62 "QParser.mly"
      (QSyntax.syntax_node)
# 2042 "QParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 62 "QParser.mly"
      (QSyntax.syntax_node)
# 2050 "QParser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_1 : (
# 62 "QParser.mly"
      (QSyntax.syntax_node)
# 2058 "QParser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv128)) : 'freshtv130)) : 'freshtv132)) : 'freshtv134)) : 'freshtv136)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)) : 'freshtv142)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "QParser.mly"
       (string)
# 2072 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (sym : (
# 40 "QParser.mly"
       (string)
# 2082 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_simple_exp = 
# 126 "QParser.mly"
            ( mk $ SymLit sym )
# 2087 "QParser.ml"
     in
    _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 39 "QParser.mly"
       (string)
# 2094 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (str : (
# 39 "QParser.mly"
       (string)
# 2104 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_simple_exp = 
# 125 "QParser.mly"
            ( mk $ StrLit str )
# 2109 "QParser.ml"
     in
    _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 33 "QParser.mly"
       (int list)
# 2116 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (elts : (
# 33 "QParser.mly"
       (int list)
# 2126 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 155 "QParser.mly"
 ( match List.map (fun s -> mk $ ShortLit s) elts with 
  | [s] -> s 
  | ss -> mk $ ArrExp ss
  )
# 2134 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 26 "QParser.mly"
       (int)
# 2141 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 26 "QParser.mly"
       (int)
# 2151 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 138 "QParser.mly"
             ( mk $ ShortLit i )
# 2156 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "QParser.mly"
       (float list)
# 2163 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (elts : (
# 30 "QParser.mly"
       (float list)
# 2173 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 145 "QParser.mly"
  ( match List.map (fun r -> mk $ RealLit r) elts with 
    | [r] -> r
    | rs -> mk $ ArrExp rs
  )
# 2181 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 24 "QParser.mly"
       (float)
# 2188 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (f : (
# 24 "QParser.mly"
       (float)
# 2198 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 134 "QParser.mly"
           ( mk $ RealLit f )
# 2203 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv113 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BINOP _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | BIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | BIT_VEC _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | FLOAT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | FLOAT_VEC _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ID _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | INT_VEC _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LONG _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LONG_VEC _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | REAL _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | REAL_VEC _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | SHORT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | SHORT_VEC _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv114)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "QParser.mly"
       (Int64.t list)
# 2259 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (elts : (
# 31 "QParser.mly"
       (Int64.t list)
# 2269 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 160 "QParser.mly"
 ( match List.map (fun s -> mk $ LongLit s) elts with 
    | [s] -> s
    | ss -> mk $ ArrExp ss
  )
# 2277 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv112)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "QParser.mly"
       (Int64.t)
# 2284 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 27 "QParser.mly"
       (Int64.t)
# 2294 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 137 "QParser.mly"
           ( mk $ LongLit i )
# 2299 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "QParser.mly"
       (string)
# 2306 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (filename : (
# 53 "QParser.mly"
       (string)
# 2316 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_stmt = 
# 84 "QParser.mly"
                ( mk $ LoadExp filename )
# 2321 "QParser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv108)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOL ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState10 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState12 in
            ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_ID__ = 
# 57 "standard.mly"
    ( [] )
# 2354 "QParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_ID__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv102)) : 'freshtv104)
    | SEMICOLON ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv106)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "QParser.mly"
       (Int32.t list)
# 2373 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (elts : (
# 32 "QParser.mly"
       (Int32.t list)
# 2383 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 150 "QParser.mly"
 ( match List.map (fun i -> mk $ IntLit i) elts with 
    | [i] -> i
    | is -> mk $ ArrExp is
  )
# 2391 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv98)

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "QParser.mly"
       (Int32.t)
# 2398 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 25 "QParser.mly"
       (Int32.t)
# 2408 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 136 "QParser.mly"
           ( mk $ IntLit i )
# 2413 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv96)

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 38 "QParser.mly"
       (string)
# 2420 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv93 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2429 "QParser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BINOP_AMEND _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2438 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 44 "QParser.mly"
       (string)
# 2443 "QParser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2451 "QParser.ml"
        )) * (
# 44 "QParser.mly"
       (string)
# 2455 "QParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv82)) : 'freshtv84)
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2504 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2511 "QParser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BINOP _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | BIT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | BIT_VEC _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | FLOAT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | FLOAT_VEC _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | ID _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | INT_VEC _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LCURLY ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LONG _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LONG_VEC _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | REAL _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | REAL_VEC _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | SHORT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | SHORT_VEC _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv86)) : 'freshtv88)
    | BINOP _ | BIT _ | BIT_VEC _ | EOF | EOL | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LBRACKET | LCURLY | LONG _ | LONG_VEC _ | LPAREN | RBRACKET | RCURLY | REAL _ | REAL_VEC _ | RPAREN | SEMICOLON | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2560 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : 'tv_simple_exp = 
# 124 "QParser.mly"
           ( mk $ Id id )
# 2566 "QParser.ml"
         in
        _menhir_goto_simple_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2576 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)) : 'freshtv94)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 29 "QParser.mly"
       (float list)
# 2584 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (elts : (
# 29 "QParser.mly"
       (float list)
# 2594 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 140 "QParser.mly"
  ( match List.map (fun f -> mk $ FloatLit f) elts with 
    | [f] -> f
    | fs -> mk $ ArrExp fs
  )
# 2602 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv80)

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "QParser.mly"
       (float)
# 2609 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (f : (
# 23 "QParser.mly"
       (float)
# 2619 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 135 "QParser.mly"
            ( mk $ FloatLit f )
# 2624 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 36 "QParser.mly"
       (int list)
# 2631 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (bits : (
# 36 "QParser.mly"
       (int list)
# 2641 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 133 "QParser.mly"
                   ( mk $ ArrExp (List.map (fun x -> mk (BitLit x)) bits)  )
# 2646 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv76)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 35 "QParser.mly"
       (int)
# 2653 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (b : (
# 35 "QParser.mly"
       (int)
# 2663 "QParser.ml"
    )) = _v in
    ((let _v : 'tv_num_or_vec = 
# 132 "QParser.mly"
          ( mk $ BitLit b )
# 2668 "QParser.ml"
     in
    _menhir_goto_num_or_vec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv74)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 42 "QParser.mly"
       (string)
# 2675 "QParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv71 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BINOP _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | BIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | BIT_VEC _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FLOAT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | FLOAT_VEC _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | ID _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | INT_VEC _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LONG _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LONG_VEC _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | REAL _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | REAL_VEC _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | SHORT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | SHORT_VEC _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv72)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv13 * _menhir_state * 'tv_list_EOL_) * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_list_EOL_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv22)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv33 * _menhir_state) * _menhir_state * 'tv_option_nonempty_list_sep__) * _menhir_state * 'tv_compound_exp) * _menhir_state * 'tv_nonempty_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2798 "QParser.ml"
        )) * (
# 44 "QParser.mly"
       (string)
# 2802 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 2811 "QParser.ml"
        )) * _menhir_state * (
# 43 "QParser.mly"
       (string)
# 2815 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_simple_exp) * _menhir_state * (
# 42 "QParser.mly"
       (string)
# 2824 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv43 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) * 'tv_args_sep) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * 'tv_simple_exp) * _menhir_state) * _menhir_state * 'tv_compound_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_simple_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * 'tv_simple_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2858 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv55 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) * _menhir_state * 'tv_option_nonempty_list_sep__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv59 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_formals_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv61 * _menhir_state) * _menhir_state) * _menhir_state * (
# 38 "QParser.mly"
       (string)
# 2882 "QParser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv70)

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_EOL_ = 
# 114 "standard.mly"
    ( [] )
# 2911 "QParser.ml"
     in
    _menhir_goto_list_EOL_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOL ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | BACKSLASH_T | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LOAD _ | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv12)

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      }

and interp_line : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 62 "QParser.mly"
      (QSyntax.syntax_node)
# 2947 "QParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BACKSLASH_T ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BINOP _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | BIT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | BIT_VEC _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FLOAT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FLOAT_VEC _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ID _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT_VEC _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LCURLY ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LOAD _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LONG _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LONG_VEC _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REAL _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | REAL_VEC _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SHORT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SHORT_VEC _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_stmt__ = 
# 57 "standard.mly"
    ( [] )
# 3006 "QParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_stmt__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv8)) : 'freshtv10))

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 61 "QParser.mly"
      (QSyntax.syntax_node)
# 3017 "QParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOL ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | BACKSLASH_T | BINOP _ | BIT _ | BIT_VEC _ | FLOAT _ | FLOAT_VEC _ | ID _ | INT _ | INT_VEC _ | LCURLY | LOAD _ | LONG _ | LONG_VEC _ | LPAREN | REAL _ | REAL_VEC _ | SHORT _ | SHORT_VEC _ | STR _ | SYM _ ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv2)) : 'freshtv4))



