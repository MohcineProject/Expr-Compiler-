

type command = 
  | Swap
  | Pop
  | Push of element
  | Add
  | Mul
  | Sub
  | Div
  | Rem
  | Exec
  | Get

and element=
  | Int of int
  | Sequence of command list 



type program = int * command list




(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)




let rec string_of_command = function
  | Swap  -> "swap" 
  | Push a -> 
    begin
      match a with 
      | Int a -> "Push " ^ (string_of_int a) 
      | Sequence cmds -> "Push " ^  "[ " ^ (String.concat " " (List.map string_of_command cmds)) ^ " ]"
    end 
  | Pop  -> "pop" 
  | Add  ->"add"
  | Mul  ->"mul"
  | Sub  ->"sub" 
  | Div  ->"div"
  | Rem  ->"rem"
  | Get  ->"get"
  | Exec  ->"exec"

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_element  = function 
    | Int i -> string_of_int i
    | Sequence cmds ->  "[ " ^ (String.concat " " (List.map string_of_command cmds)) ^ " ]"


let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

