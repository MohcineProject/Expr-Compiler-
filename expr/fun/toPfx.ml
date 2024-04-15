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

    let rec generate env = function
    | Const a ->
        let new_env = List.map (fun (v, d) -> (v, d + 1)) env in
        [Push (Int a)]
    | Uminus a -> List.concat [generate env a; [Push (Int 0)]; [Sub]]
    | Binop(op, a, b) -> List.concat [generate env b; generate env a; [op_to_Pfx op]]
    | Var x ->
        begin
          match List.assoc_opt x env with
          | Some index -> [Push (Int index); Get]
          | None -> failwith "Variable not found in environment"
        end
    | Fun (x, expr) ->
        let new_env = (x, 0) :: List.map (fun (v, d) -> (v, d + 1)) env in
        [Push (Sequence (Get :: (generate new_env expr)))]
    | App (expr1, expr2) -> List.concat [generate env expr1; generate env expr2; [Exec]]
    | _ -> failwith "Not supported yet"
  
