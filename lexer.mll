{
open Parser
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = alpha(alpha|digit|'_')*

rule token = parse
| [' ' '\t' '\n']+ { token lexbuf }
| "let" { LET }
| ":=" { DEF }
| "in" { IN }
| "(" { LPAREN }
| ")" { RPAREN }
| "^" { LAMBDA }
| "." { DOT }
| ":" { COLON }
| "int" { INT }
| "->" { ARROW }
| ident { IDENT(Lexing.lexeme lexbuf) }
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
