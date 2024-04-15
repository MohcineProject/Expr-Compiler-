open Ast
open Printf

let string_of_stack stack =  
  sprintf "[%s]" (String.concat ";" (List.map string_of_element stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))


(*New functions*)
let  reverse lst = 
let rec reverse acc = function 
  | [] -> [] 
  | x :: rest -> reverse (x::acc) rest 
in reverse [] lst  


let rec get_i_th_element (left ,right) i = 
  match (left,right) with 
  | (left, s::tl) when i > 1 -> get_i_th_element (s::left , tl) (i-1) 
  | (left, s::tl) when i == 1 ->  Ok (left, s::tl) 
  | (left, []) when i > 0 -> Error("Index out of range" , (left ,right))
  |  _ -> Error ("Invalid syntax" , (left,right))



(* Question 4.2 *)
let step state =
  match state with
  (* Valid configurations *)

  | Swap :: q , v:: p::stack -> Ok (q,  p::  v::stack)
  | Pop :: q , _::stack -> Ok (q, stack)
  | Push a:: q , stack -> Ok (q, a::stack)

  | Add :: q , Int v:: Int p::stack -> Ok (q, Int (p+v)::stack)
  | Add :: _q , _:: _ ::_stack -> Error("Bad inputs for arithmetic operation " , state) 

  | Sub :: q , Int v::Int p::stack -> Ok (q, Int (v-p)::stack)
  | Sub :: _q , _:: _ ::_stack -> Error("Bad inputs for arithmetic operation " , state) 

  | Mul :: q , Int v::Int p::stack -> Ok (q, Int (v*p)::stack)
  | Mul :: _q , _:: _ ::_stack -> Error("Bad inputs for arithmetic operation " , state) 

  | Rem :: q , Int v::Int p::stack -> Ok (q, Int (v mod p)::stack)
  | Rem :: _q , _:: _ ::_stack -> Error("Bad inputs for arithmetic operation " , state) 

  | Div :: _ , Int _v::Int 0::_ -> Error("Div with a 0 ", state)
  | Div :: q , Int v:: Int p ::stack -> Ok (q, (Int (v/p))::stack)
  | Div :: _q , _:: _ ::_stack -> Error("Bad inputs for arithmetic operation " , state) 


  | Get::q , Int i::stack ->
    begin
        match (get_i_th_element ([],  stack) i)  with
        |Ok (left, a::right) -> Ok (q, a::(reverse left @ right))
        | Error (msg , _ ) -> Error ( msg , state ) 
        | _ -> Error("Unkonwn Error occured in the get operation" , state)
    end
  | Get::_q , _::_stack -> Error ( "Get invalid input ", state )
  | Exec::q , Sequence cmds ::stack -> Ok(cmds @ q , stack) 
  | Exec::_q , _::_stack -> Error("Invalid input for exec " , state)   


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
    | Ok(Some result) -> printf "= %s\n" (string_of_element result)
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
