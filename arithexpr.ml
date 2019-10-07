(**************************************)
(*                                    *)
(*          Module ArithExpr          *)
(*                                    *)
(**************************************)

module type ARITHEXPR =
sig

  type arith_expr

  val add : (arith_expr * arith_expr) -> arith_expr
  val sub : (arith_expr * arith_expr) -> arith_expr
  val mul : (arith_expr * arith_expr) -> arith_expr
  val div : (arith_expr * arith_expr) -> arith_expr
  val modulo : (arith_expr * arith_expr) -> arith_expr
  val value : Bigint.Bigint.bigint -> arith_expr

  val string_of_arith_expr : arith_expr -> string

  val solve_arith_expr : arith_expr -> Bigint.Bigint.bigint

end

module ArithExpr : ARITHEXPR =
struct

  (**************************)
  (*          Type          *)
  (**************************)

  type arith_expr =
    | Add of (arith_expr * arith_expr)
    | Sub of (arith_expr * arith_expr)
    | Mul of (arith_expr * arith_expr)
    | Div of (arith_expr * arith_expr)
    | Modulo of (arith_expr * arith_expr)
    | Value of Bigint.Bigint.bigint

  let add (x, y) = Add (x, y)
  let sub (x, y) = Sub (x, y)
  let mul (x, y) = Mul (x, y)
  let div (x, y) = Div (x, y)
  let modulo (x, y) = Modulo (x, y)
  let value i = Value (i)

  (*******************************)
  (*          Functions          *)
  (*******************************)

  let rec string_of_arith_expr = function
    | Add(expr1, expr2) -> "("^(string_of_arith_expr expr1)^" + "^(string_of_arith_expr expr2)^")"
    | Sub(expr1, expr2) -> "("^(string_of_arith_expr expr1)^" - "^(string_of_arith_expr expr2)^")"
    | Mul(expr1, expr2) -> "("^(string_of_arith_expr expr1)^" * "^(string_of_arith_expr expr2)^")"
    | Div(expr1, expr2) -> "("^(string_of_arith_expr expr1)^" / "^(string_of_arith_expr expr2)^")"
    | Modulo(expr1, expr2) -> "("^(string_of_arith_expr expr1)^" % "^(string_of_arith_expr expr2)^")"
    | Value(i) -> Bigint.Bigint.string_of_bigint i

  let rec solve_arith_expr = function
    | Add(expr1, expr2) -> Bigint.Bigint.add (solve_arith_expr expr1) (solve_arith_expr expr2)
    | Sub(expr1, expr2) -> Bigint.Bigint.sub (solve_arith_expr expr1) (solve_arith_expr expr2)
    | Mul(expr1, expr2) -> Bigint.Bigint.mul (solve_arith_expr expr1) (solve_arith_expr expr2)
    | Div(expr1, expr2) -> Bigint.Bigint.div (solve_arith_expr expr1) (solve_arith_expr expr2)
    | Modulo(expr1, expr2) -> Bigint.Bigint.modulo (solve_arith_expr expr1) (solve_arith_expr expr2)
    | Value(i) -> i

end
