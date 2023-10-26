
let rec loop () =
  let _ = Printf.printf "> " in
  let line = read_line () in
  match line with
  | "" -> ()
  | line ->
    let lexer = Lexer.init line in
    let _, ast = Parser.parse lexer in
    let _ = Printf.printf "= %d\n" (Ast.eval ast) in
    loop ()

let _ = loop ()
