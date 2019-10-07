{ open Parser }

let hexa = ("0x")['0'-'9''A'-'F''a'-'f']+
let dec = ['0'-'9']+
let oct = ("0")['0'-'7']+
let bin = ("0b")['0'-'1']+

rule token = parse
  | ' '|'\t'	{ token lexbuf }
  | hexa	{ BIGINT(Bigint.Bigint.bigint_of_string (Lexing.lexeme lexbuf)) }
  | dec		{ BIGINT(Bigint.Bigint.bigint_of_string (Lexing.lexeme lexbuf)) }
  | oct		{ BIGINT(Bigint.Bigint.bigint_of_string (Lexing.lexeme lexbuf)) }
  | bin		{ BIGINT(Bigint.Bigint.bigint_of_string (Lexing.lexeme lexbuf)) }
  | '('		{ PARO }
  | ')'		{ PARC }
  | '+'		{ ADD }
  | '-'		{ SUB }
  | '*'		{ MUL }
  | '/'		{ DIV }
  | '%'		{ MODULO }
  | '\n'	{ EOL }
  | "quit"	{ QUIT }
