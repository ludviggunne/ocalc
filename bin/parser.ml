
open Token
open Lexer
open Ast

let rec parse lexer = sum lexer

and sum lexer =
  let rec take lexer left =
    match next lexer with
    | _, None -> lexer, left
    | next_lexer, Some tok ->
      match tok with
      | Plus | Minus ->
        let next_lexer, right = product next_lexer in
        take next_lexer (Binary (left, tok, right))
      | _ -> lexer, left
  in let lexer, left = product lexer
  in take lexer left

and product lexer =
  let rec take lexer left =
    match next lexer with
    | _, None -> lexer, left
    | next_lexer, Some tok ->
      match tok with
      | Asterix | Slash ->
        let next_lexer, right = factor next_lexer in
        take next_lexer (Binary (left, tok, right))
      | _ -> lexer, left
  in let lexer, left = factor lexer
  in take lexer left

and factor lexer =
  match next lexer with
  | _, None -> failwith "Empty expression"
  | next_lexer, Some tok ->
    match tok with
    | Num x -> next_lexer, Atomic x
    | Minus ->
      let next_lexer, operand = factor next_lexer in
      next_lexer, Neg operand
    | LeftParenthesis ->
      (let next_lexer, expr = sum next_lexer in
      match next next_lexer with
      | _, None -> failwith "Missing closing parenthesis"
      | next_lexer, Some tok ->
        (match tok with
        | RightParenthesis -> next_lexer, expr
        | _ -> failwith "Missing closing parenthesis"))
    | _ -> failwith "Unexpected token"
;;
