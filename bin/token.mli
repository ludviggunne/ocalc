
type t =
  | Plus
  | Minus
  | Asterix
  | Slash
  | LeftParenthesis
  | RightParenthesis
  | Num of int

val tokstr : t -> string
