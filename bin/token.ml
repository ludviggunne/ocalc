
type t =
  | Plus
  | Minus
  | Asterix
  | Slash
  | LeftParenthesis
  | RightParenthesis
  | Num of int

let tokstr token =
  match token with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Asterix -> "Asterix"
  | Slash -> "Slash"
  | LeftParenthesis -> "LeftParenthesis"
  | RightParenthesis -> "RightParenthesis"
  | Num _ -> "Num"
