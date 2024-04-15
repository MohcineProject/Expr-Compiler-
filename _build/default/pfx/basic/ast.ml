

type command = 
  | Swap
  | Pop
  | Push of int
  | Add
  | Mul
  | Sub
  | Div
  | Rem



  
type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)

let string_of_command = function
  | Swap  -> "swap" 
  | Push a -> "push " ^ string_of_int a 
  | Pop  -> "pop" 
  | Add  ->"add"
  | Mul  ->"mul"
  | Sub  ->"sub" 
  | Div  ->"div"
  | Rem  ->"rem"

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

