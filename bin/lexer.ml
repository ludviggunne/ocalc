
type t =
  {
  source : string;
  position : int;
  peeked : Token.t option
  }
;;

let init source = { source; position = 0; peeked = None }
;;

let rec next lexer =
  match lexer.peeked with
  | Some peeked ->
    {
      source = lexer.source;
      position = lexer.position;
      peeked = None
    }, Some peeked
  | None ->
    let lexer = trim lexer in
    let open Token in
    match get lexer with
    | None -> lexer, None
    | Some ch -> match ch with
      | '+' -> advance lexer, Some Plus
      | '-' -> advance lexer, Some Minus
      | '*' -> advance lexer, Some Asterix
      | '/' -> advance lexer, Some Slash
      | '(' -> advance lexer, Some LeftParenthesis
      | ')' -> advance lexer, Some RightParenthesis
      | '0'..'9' -> number lexer
      | _ -> failwith "Illegal character"

and number lexer =
  let pos = lexer.position in
  let rec iter lexer =
    match get lexer with
    | None -> lexer
    | Some ch -> match ch with
      | '0'..'9' -> iter (advance lexer)
      | _ -> lexer in
  let lexer = iter lexer in
  let substr = String.sub lexer.source pos (lexer.position - pos) in
  let open Token in
  lexer, Some (Num (int_of_string substr))

and trim lexer =
  match get lexer with
  | None -> lexer
  | Some ch -> match ch with
    | ' ' -> advance lexer
    | _ -> lexer

and get lexer =
  if lexer.position == String.length lexer.source
  then None
  else Some (String.get lexer.source lexer.position)

and advance lexer =
  {
  source = lexer.source;
  position = lexer.position + 1;
  peeked = lexer.peeked;
  }
;;

let peek lexer =
  match lexer.peeked with
  | Some peeked -> lexer, Some peeked
  | None -> next lexer
