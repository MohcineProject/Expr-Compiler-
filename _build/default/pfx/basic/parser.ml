
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | SWAP
    | SUB
    | REM
    | PUSH of (
# 14 "pfx/basic/parser.mly"
       (int)
# 17 "pfx/basic/parser.ml"
  )
    | POP
    | MUL
    | INT of (
# 20 "pfx/basic/parser.mly"
       (int)
# 24 "pfx/basic/parser.ml"
  )
    | EOF
    | DIV
    | ADD
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState10
  | MenhirState5
  | MenhirState1

# 1 "pfx/basic/parser.mly"
  
  (* Ocaml code here*)
open Ast


# 52 "pfx/basic/parser.ml"

let rec _menhir_goto_command : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.command list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (o : (Ast.command))), _, (c : (Ast.command list))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 41 "pfx/basic/parser.mly"
                         (o::c)
# 65 "pfx/basic/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (o : (
# 14 "pfx/basic/parser.mly"
       (int)
# 74 "pfx/basic/parser.ml"
        ))), _, (c : (Ast.command list))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 42 "pfx/basic/parser.mly"
                     ((Push o)::c )
# 79 "pfx/basic/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (i : (
# 20 "pfx/basic/parser.mly"
       (int)
# 93 "pfx/basic/parser.ml"
            ))), _, (c : (Ast.command list))) = _menhir_stack in
            let _v : (Ast.program) = 
# 39 "pfx/basic/parser.mly"
                       ((i,c))
# 98 "pfx/basic/parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.command) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | DIV ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | MUL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | POP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | PUSH _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | REM ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SWAP ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (o : (Ast.command))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 43 "pfx/basic/parser.mly"
               ([o])
# 137 "pfx/basic/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 52 "pfx/basic/parser.mly"
       (Swap)
# 167 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 49 "pfx/basic/parser.mly"
        (Sub)
# 178 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 51 "pfx/basic/parser.mly"
       (Rem)
# 189 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "pfx/basic/parser.mly"
       (int)
# 196 "pfx/basic/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DIV ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MUL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | POP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PUSH _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | REM ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SWAP ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (o : (
# 14 "pfx/basic/parser.mly"
       (int)
# 224 "pfx/basic/parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 44 "pfx/basic/parser.mly"
            ([Push o])
# 229 "pfx/basic/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 46 "pfx/basic/parser.mly"
      (Pop)
# 244 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 48 "pfx/basic/parser.mly"
        (Mul)
# 255 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> (Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.program)) = _v in
    Obj.magic _1

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 50 "pfx/basic/parser.mly"
        (Div)
# 273 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 47 "pfx/basic/parser.mly"
       (Add)
# 284 "pfx/basic/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | DIV ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState1 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, (i : (
# 20 "pfx/basic/parser.mly"
       (int)
# 329 "pfx/basic/parser.ml"
            ))) = _menhir_stack in
            let _v : (Ast.program) = 
# 37 "pfx/basic/parser.mly"
                   ( i,[] )
# 334 "pfx/basic/parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _v
        | MUL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | POP ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | PUSH _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | REM ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | SWAP ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)

# 57 "pfx/basic/parser.mly"
  

# 362 "pfx/basic/parser.ml"

# 269 "<standard.mly>"
  

# 367 "pfx/basic/parser.ml"
