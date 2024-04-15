{
  open Parser 
  open Ast
  open Utils.Location 

 
  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb) 

let tokenize_command_str str =
  let lexbuf = Lexing.from_string str in
  let rec tokenize acc =
    try 
      let tok = Lexer.token lexbuf in
      match tok with
      | SWAP -> tokenize (SWAP :: acc)
      | POP -> tokenize (POP :: acc)
      | PUSH a -> tokenize (PUSH a :: acc)
      | ADD -> tokenize (ADD :: acc)
      | MUL -> tokenize (MUL :: acc)
      | SUB -> tokenize (SUB :: acc)
      | DIV -> tokenize (DIV :: acc)
      | REM -> tokenize (REM :: acc)
      | GET -> tokenize (GET :: acc)
      | EXEC -> tokenize (EXEC :: acc)
      | EOF -> tokenize (EOF :: acc)
      | INT a -> tokenize (INT a :: acc)
    with 
    | Failure msg  ->  failwith (Printf.sprintf "Error located at '%s'" (string_of (curr lexbuf) )) 
  in
  tokenize []




  let print_token = function
|SWAP        -> print_string "Swap" 
|  POP   -> print_string "Pop"
| PUSH a -> print_string ("Push " ^ (string_of_element a)  )
|  ADD   -> print_string "Add"
|  MUL  -> print_string "Mul"
|  SUB-> print_string "Sub"
|  DIV  -> print_string "Div"
|  REM   -> print_string "Rem"
| GET -> print_string "Get" 
| EXEC -> print_string "Exec"
| EOF->  print_string "\n"
| INT a -> print_string (string_of a )
| _ -> failwith (Printf.sprintf "Not a token ")

 }

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let integer = ['0'-'9']+
let command = "Swap"|"Sub"|"Div"|"Mul"|"Rem"|"Add"|"Pop"





rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }

  (* integers *)

  | integer as nb       {  (mk_int nb ) }
  (***** TO COMPLETE *****)
  (* commands  *)
  | "Swap" { SWAP }
  | "Sub" { SUB }
  | "Div" { DIV }
  | "Mul" { MUL }
  | "Rem" { REM }
  | "Add" { ADD }
  | "Pop" { POP }
  | "Push" blank+ (integer+ as nb )  { PUSH ( int_of_string nb)}
  | "Push" blank+ "[" blank* ((command blank+)* as cmds ) "]"   { PUSH (tokenize_command_str  cmds) }
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "\nIllegal character '%c' \n error located at '%s'" c ( string_of (curr lexbuf) ) )  }

  
