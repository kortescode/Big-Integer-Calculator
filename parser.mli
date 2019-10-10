
(* The type of tokens. *)

type token = 
  | SUB
  | QUIT
  | PARO
  | PARC
  | MUL
  | MODULO
  | EOL
  | DIV
  | BIGINT of (Bigint.Bigint.bigint)
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Arithexpr.ArithExpr.arith_expr)
