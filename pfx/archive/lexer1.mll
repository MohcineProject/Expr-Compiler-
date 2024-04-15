{
  (*open Parser*)
  open Utils.Location 


  type token =
 | EOF | PLUS | MINUS | TIMES | DIV | MOD | LPAR | RPAR
 | INT of int | IDENT of string 
  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)

     let print_token = function
    | EOF -> print_string "EOF"
    | PLUS -> print_string "PLUS"
    | MINUS -> print_string "MINUS"
    | TIMES -> print_string "TIMES"
    | DIV -> print_string "DIV"
    | MOD -> print_string "MOD"
    | LPAR -> print_string "LPAR"
    | RPAR -> print_string "RPAR"
    | INT i -> print_int i
    | IDENT s -> print_string s
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

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
  | digit+ as nb           { mk_int nb }
  
  (***** TO COMPLETE *****)
  (* commands  *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "*" { TIMES }
  | "%" { MOD }
  | "(" { LPAR }
  | ")" { RPAR }
  (* identifiers *)
  | letter (letter | digit | '_')* as id { IDENT id }
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "\nIllegal character '%c' \n error located at '%s'" c ( string_of (curr lexbuf) ) )  }

  
