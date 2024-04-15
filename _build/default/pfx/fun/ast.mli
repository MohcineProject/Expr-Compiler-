(* The type of the commands for the stack machine *)
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

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

val string_of_element : element -> string
(* Converting a program to a string for printing *)
val string_of_program : program -> string
