{ open Parser }
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = alpha(alpha|digit|'_')*
rule token = parse
| [' ' '\t' '\n']+         { token lexbuf }
| "let" { LET    } | "in"  { IN     } | "int" { INT    }
| ":="  { DEF    } | "("   { LPAREN } | ")"   { RPAREN }
| "^"   { LAMBDA } | "."   { DOT    } | ":"   { COLON  }
| "->"  { ARROW  } | eof   { EOF    }
| ident { IDENT(Lexing.lexeme lexbuf) }
| _     { failwith (Printf.sprintf "unknown token %s near characters %d-%d"
                    (Lexing.lexeme lexbuf)
                    (Lexing.lexeme_start lexbuf)
                    (Lexing.lexeme_end lexbuf)) }
