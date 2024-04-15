open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  (* Valid configurations *)
  | Swap :: q , v::p::stack -> Ok (q, p::v::stack)
  | Add :: q , v::p::stack -> Ok (q, (p+v)::stack)
  | Sub :: q , v::p::stack -> Ok (q, (v-p)::stack)
  | Mul :: q , v::p::stack -> Ok (q, (v*p)::stack)
  | Rem :: q , v::p::stack -> Ok (q, (v mod p)::stack)
  | Div :: _q , _v::0::_ -> Error("Div with a 0 ", state)
  | Div :: q , v::p::stack -> Ok (q, (v/p)::stack)
  | Pop :: q , _v::stack -> Ok (q, stack)
  | Push a :: q , stack -> Ok (q, a::stack)
  | [], _ -> Error("Nothing to step",state)
  | Pop :: _q , [] -> Error("Pop from an empty list", state)
  | _ :: _q , _a::[] -> Error ("Operand with a single element", state)
  | _ :: _q , [] -> Error ("Operand an empty list", state)

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
