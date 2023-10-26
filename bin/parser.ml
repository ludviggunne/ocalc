
open Token
open Ast

let rec parse lexer = sum lexer

and sum lexer =
  let rec take lexer left =
    match Lexer.next lexer with
    | _, None -> lexer, left
    | lexer, Some tok ->
      match tok with
      | Plus | Minus ->
        let lexer, right = product lexer in
        take lexer (Binary (left, tok, right))
      | _ -> lexer, left
  in let lexer, left = product lexer
  in take lexer left

and product lexer =
  let rec take lexer left =
    match Lexer.next lexer with
    | _, None -> lexer, left
    | lexer, Some tok ->
      match tok with
      | Asterix | Slash ->
        let lexer, right = factor lexer in
        take lexer (Binary (left, tok, right))
      | _ -> lexer, left
  in let lexer, left = factor lexer
  in take lexer left

and factor lexer =
  match Lexer.next lexer with
  | _, None -> failwith "Empty expression"
  | lexer, Some tok ->
    match tok with
    | Num x -> lexer, Atomic x
    | Minus ->
      let lexer, operand = factor lexer in
      lexer, Neg operand
    | LeftParenthesis ->
      (let lexer, expr = sum lexer in
      match Lexer.next lexer with
      | _, None -> failwith "Missing closing parenthesis"
      | lexer, Some tok ->
        (match tok with
        | RightParenthesis -> lexer, expr
        | _ -> failwith "Missing closing parenthesis"))
    | _ -> failwith "Unexpected token"
;;
