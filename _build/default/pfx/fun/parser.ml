
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | SWAP
    | SUB
    | REM
    | PUSH of (
# 15 "pfx/fun/parser.mly"
       (Ast.element)
# 17 "pfx/fun/parser.ml"
  )
    | POP
    | MUL
    | INT of (
# 21 "pfx/fun/parser.mly"
       (int)
# 24 "pfx/fun/parser.ml"
  )
    | GET
    | EXEC
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
  | MenhirState12
  | MenhirState5
  | MenhirState1

# 1 "pfx/fun/parser.mly"
  
  (* Ocaml code here*)
open Ast



# 55 "pfx/fun/parser.ml"

let rec _menhir_goto_command : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.command list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (o : (Ast.command))), _, (c : (Ast.command list))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 44 "pfx/fun/parser.mly"
                         (o::c)
# 68 "pfx/fun/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (o : (
# 15 "pfx/fun/parser.mly"
       (Ast.element)
# 77 "pfx/fun/parser.ml"
        ))), _, (c : (Ast.command list))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 45 "pfx/fun/parser.mly"
                     ((Push o)::c )
# 82 "pfx/fun/parser.ml"
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
# 21 "pfx/fun/parser.mly"
       (int)
# 96 "pfx/fun/parser.ml"
            ))), _, (c : (Ast.command list))) = _menhir_stack in
            let _v : (Ast.program) = 
# 42 "pfx/fun/parser.mly"
                       ((i,c))
# 101 "pfx/fun/parser.ml"
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
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DIV ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | EXEC ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | GET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MUL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | POP ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PUSH _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | REM ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SWAP ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (o : (Ast.command))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 46 "pfx/fun/parser.mly"
               ([o])
# 144 "pfx/fun/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState12 ->
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
# 56 "pfx/fun/parser.mly"
       (Swap)
# 174 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 53 "pfx/fun/parser.mly"
        (Sub)
# 185 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 55 "pfx/fun/parser.mly"
       (Rem)
# 196 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "pfx/fun/parser.mly"
       (Ast.element)
# 203 "pfx/fun/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DIV ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | EXEC ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | GET ->
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
# 15 "pfx/fun/parser.mly"
       (Ast.element)
# 235 "pfx/fun/parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 47 "pfx/fun/parser.mly"
            ([Push o])
# 240 "pfx/fun/parser.ml"
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
# 50 "pfx/fun/parser.mly"
      (Pop)
# 255 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 52 "pfx/fun/parser.mly"
        (Mul)
# 266 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 58 "pfx/fun/parser.mly"
      (Get)
# 277 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 57 "pfx/fun/parser.mly"
       (Exec)
# 288 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> (Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.program)) = _v in
    Obj.magic _1

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 54 "pfx/fun/parser.mly"
        (Div)
# 306 "pfx/fun/parser.ml"
     in
    _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 51 "pfx/fun/parser.mly"
       (Add)
# 317 "pfx/fun/parser.ml"
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
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | DIV ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState1 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, (i : (
# 21 "pfx/fun/parser.mly"
       (int)
# 362 "pfx/fun/parser.ml"
            ))) = _menhir_stack in
            let _v : (Ast.program) = 
# 40 "pfx/fun/parser.mly"
                   ( i,[] )
# 367 "pfx/fun/parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _v
        | EXEC ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | GET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
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

# 64 "pfx/fun/parser.mly"
  

# 399 "pfx/fun/parser.ml"

# 269 "<standard.mly>"
  

# 404 "pfx/fun/parser.ml"
