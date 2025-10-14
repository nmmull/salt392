{
    open Parser
    exception Error of (Lexing.position * Lexing.position)
}

let int = '-'? ['0'-'9']+
let ident = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t' '\r']+
let comment = "//" [^ '\n']*
let macro = "#[" [^ ']']* ']'

rule read =
  parse
  | "==" { BEQ }
  | "!=" { NEQ }
  | "+=" { ADDEQ }
  | "-=" { SUBEQ }
  | "*=" { MULEQ }
  | "/=" { DIVEQ }
  | "%=" { MODEQ }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<" { LT }
  | ">" { GT }
  | "||" { OR }
  | "&&" { AND }
  | "-" { SUB }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACKET }
  | "}" { RBRACKET }
  | "," { COMMA }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "->" { ARROW }
  | "&" { AMPERSAND }
  | "*" { STAR }
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | "%" { MOD }
  | "!" { NOT }
  | "." { DOT }
  | "=" { EQ }
  | "true" { TRUE }
  | "false" { FALSE }
  | "i32" { INTTY }
  | "bool" { BOOLTY }
  | "fn" { FN }
  | "let" { LET }
  | "mut" { MUT }
  | "if" { IF }
  | "else" { ELSE }
  | "Box::new" { BOX }
  | "assert!" { ASSERT }
  | eof { EOF }
  | int { INT (Int32.of_string (Lexing.lexeme lexbuf)) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | comment { read lexbuf }
  | macro { read lexbuf }
  | _ { raise (Error ((Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf))) }
