
type t =
  | Atomic of int
  | Binary of t * Token.t * t
  | Neg of t

let rec eval ast =
  match ast with
  | Atomic x -> x
  | Binary (left, op, right) ->
    (
      let left, right = (eval left), (eval right) in
      match op with
      | Plus -> left + right
      | Minus -> left - right
      | Asterix -> left * right
      | Slash -> left / right
      | _ -> failwith "Illegal binary operator"
    )
  | Neg x -> -(eval x)
