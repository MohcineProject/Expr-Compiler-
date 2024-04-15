(* The type of the commands for the stack machine *)
type command =
Swap        |
Pop  |
Push of int |
Add  |
Mul |
Sub |
Div |
Rem  


(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
