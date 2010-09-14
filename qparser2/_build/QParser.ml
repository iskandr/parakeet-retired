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

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState51
  | MenhirState41
  | MenhirState40
  | MenhirState34
  | MenhirState30
  | MenhirState29
  | MenhirState19
  | MenhirState17
  | MenhirState14
  | MenhirState9
  | MenhirState7
  | MenhirState6
  | MenhirState5
  | MenhirState0

  
  open QSyntax
  let mk = mk_syntax_node
  let ($) f x = f x 
  
let _eRR =
  Error

let rec _menhir_goto_fn : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    let _v : (QSyntax.syntax_node) =          ( f ) in
    _menhir_goto_expr1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_sep_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_sep_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BIT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | IMPLICIT_FLOAT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | IMPLICIT_INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | LCURLY ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | REAL _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | SHORT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list option) =     ( Some x ) in
        _menhir_goto_option_nonempty_list_sep__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BIT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | IMPLICIT_FLOAT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | IMPLICIT_INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | LCURLY ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | REAL _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | SHORT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_nonempty_list_sep__stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _, _) = _menhir_stack in
        let _v : (QSyntax.syntax_node list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_nonempty_list_sep__stmt_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (QSyntax.syntax_node list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_nonempty_list_sep__stmt__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (QSyntax.syntax_node list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_stmt_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (QSyntax.syntax_node list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_stmt__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_nonempty_list_sep__expr0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState6 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let items = _v in
        let _v : (QSyntax.syntax_node) =                                                ( mk $ BlockExp items ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState19 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RCURLY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _), _, ids), _, _), _, body) = _menhir_stack in
                let _v : (QSyntax.syntax_node) =   ( mk $ LamExp(ids, body) ) in
                _menhir_goto_fn _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState6 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RCURLY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, body) = _menhir_stack in
                let _v : (QSyntax.syntax_node) =                              ( mk $ SimpleLamExp body ) in
                _menhir_goto_fn _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _, _) = _menhir_stack in
        let _v : (QSyntax.syntax_node list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_nonempty_list_sep__expr0_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (QSyntax.syntax_node list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr0__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (QSyntax.syntax_node list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr0_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_app : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let e = _v in
    let _v : (QSyntax.syntax_node) =           ( e ) in
    _menhir_goto_expr0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_sep : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | SEMICOLON ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | BIT _ | FLOAT _ | ID _ | IMPLICIT_FLOAT _ | IMPLICIT_INT _ | INT _ | LCURLY | LPAREN | REAL _ | SHORT _ | STR _ | SYM _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_sep_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_expr0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BIT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | FLOAT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | IMPLICIT_FLOAT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | IMPLICIT_INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | INT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LCURLY ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | REAL _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | SHORT _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | STR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | SYM _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (QSyntax.syntax_node list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr0_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
        let _v : (QSyntax.syntax_node) =                           ( mk $ AppExp(lhs, [rhs]) ) in
        _menhir_goto_app _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 | MenhirState41 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SEMICOLON ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (QSyntax.syntax_node list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_nonempty_list_sep__expr0_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (QSyntax.syntax_node) =                           ( e ) in
            _menhir_goto_expr1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 | MenhirState60 | MenhirState0 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : (QSyntax.syntax_node) =             ( e ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState51 | MenhirState0 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BIT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | FLOAT _v ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | ID _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | IMPLICIT_FLOAT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | IMPLICIT_INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | INT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | LCURLY ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | LPAREN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | REAL _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | SHORT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | STR _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | SYM _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | _ ->
                    assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
            | EOL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (QSyntax.syntax_node list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_SEMICOLON_stmt_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState60 | MenhirState58 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | SEMICOLON ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (QSyntax.syntax_node list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_nonempty_list_sep__stmt_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr0__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : (QSyntax.syntax_node list) = let args =
      let xs = xs0 in
          ( xs )
    in
                                               ( args ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, lhs), _), _, args) = _menhir_stack in
        let _v : (QSyntax.syntax_node) =   ( mk $ AppExp(lhs, args) ) in
        _menhir_goto_app _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_nonempty_list_sep__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BIT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IMPLICIT_FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IMPLICIT_INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LCURLY ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | REAL _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | SHORT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =             () in
    _menhir_goto_sep _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) =       () in
    _menhir_goto_sep _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BIT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IMPLICIT_FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IMPLICIT_INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState29 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BIT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | FLOAT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IMPLICIT_FLOAT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IMPLICIT_INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | INT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | LCURLY ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | REAL _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | SHORT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | STR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | SYM _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState30 in
            let _v : (QSyntax.syntax_node list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr0__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | LCURLY ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | REAL _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | SHORT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | EOF | EOL | RBRACKET | RCURLY | RPAREN | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : (QSyntax.syntax_node) =             ( e) in
        _menhir_goto_expr0 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : (string list) = let names =
      let xs = xs0 in
          ( xs )
    in
                                             ( names ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EOL ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SEMICOLON ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | BIT _ | FLOAT _ | ID _ | IMPLICIT_FLOAT _ | IMPLICIT_INT _ | INT _ | LCURLY | LPAREN | REAL _ | SHORT _ | STR _ | SYM _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState14 in
            let _v : (unit list option) =     ( None ) in
            _menhir_goto_option_nonempty_list_sep__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_lit : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let e = _v in
    let _v : (QSyntax.syntax_node) =           ( e ) in
    _menhir_goto_expr1 _menhir_env _menhir_stack _menhir_s _v

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

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_stmt__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, xs0) = _menhir_stack in
        let _v : (QSyntax.syntax_node) = let items =
          let xs = xs0 in
              ( xs )
        in
                                                    ( mk $ BlockExp items) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_loption_separated_nonempty_list_nonempty_list_sep__stmt__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (QSyntax.syntax_node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, xs0) = _menhir_stack in
        let _v : (QSyntax.syntax_node) = let items =
          let xs = xs0 in
              ( xs )
        in
                                               ( mk $ BlockExp items ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let sym = _v in
    let _v : (QSyntax.syntax_node) =             ( mk $ SymLit sym ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let str = _v in
    let _v : (QSyntax.syntax_node) =             ( mk $ StrLit str ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (QSyntax.syntax_node) =              ( mk $ ShortLit i ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    let _v : (QSyntax.syntax_node) =            ( mk $ RealLit f ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BIT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IMPLICIT_FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IMPLICIT_INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LCURLY ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | REAL _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | SHORT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BIT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IMPLICIT_FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IMPLICIT_INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState6 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState7 in
            let _v : (string list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_SEMICOLON_ID__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | LCURLY ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | REAL _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | SHORT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Int32.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (QSyntax.syntax_node) =           ( mk $ IntLit i ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Int32.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (QSyntax.syntax_node) =                    ( mk $ IntLit i ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    let _v : (QSyntax.syntax_node) =                      ( mk $ FloatLit f ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let id = _v in
    let _v : (QSyntax.syntax_node) =            ( mk $ Id id ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    let _v : (QSyntax.syntax_node) =             ( mk $ FloatLit f ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _v : (QSyntax.syntax_node) =           ( mk $ BitLit b ) in
    _menhir_goto_lit _menhir_env _menhir_stack _menhir_s _v

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

and interp_line : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (QSyntax.syntax_node) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BIT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IMPLICIT_FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IMPLICIT_INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LCURLY ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REAL _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SHORT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _v : (QSyntax.syntax_node list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_stmt__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (QSyntax.syntax_node) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BIT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | FLOAT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IMPLICIT_FLOAT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IMPLICIT_INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | INT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LCURLY ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | REAL _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | SHORT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | STR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | SYM _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState58 in
        let _v : (QSyntax.syntax_node list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_nonempty_list_sep__stmt__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(>=) _menhir_env._menhir_shifted 0);
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)



