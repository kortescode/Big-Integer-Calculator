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

module ArithExpr : ARITHEXPR
