{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let letter = ['a'-'z' 'A'-'Z']
let id = letter+

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let int = '-'? ['0'-'9'] ['0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPARENS }
  | ")" { RPARENS }
  | "->" { ARROW }
  | "fun" { FUN }
  | id { IDENT (Lexing.lexeme lexbuf) }
  | "+" { PLUS }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }