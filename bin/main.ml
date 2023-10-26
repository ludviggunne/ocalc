
let lexer = Lexer.init "10 * - 2"
let _, ast = Parser.parse lexer
let () = Printf.printf "%d\n" (Ast.eval ast)
