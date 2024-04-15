open Ast
open BasicPfx.Ast 
open BinOp


  let op_to_Pfx = 
    function 
    | Badd -> Add  
    | Bsub -> Sub
    | Bmul -> Mul
    | Bdiv -> Div
    | Bmod -> Rem 

let rec generate = function
  | Const a -> [Push a]
  | Uminus a -> List.concat [generate a ; [Push 0]; [Sub]]
  | Binop(op, a, b) -> List.concat [ generate b; generate a; [op_to_Pfx op]]
  | _ -> failwith "Not supported yet"

