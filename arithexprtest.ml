(******************************************)
(*                                        *)
(*          Module ArithExprTest          *)
(*                                        *)
(******************************************)

module type ARITHEXPRTEST =
sig

  val launch_tests : unit -> unit

end

module ArithExprTest : ARITHEXPRTEST =
struct

  let launch_tests () =
    let var1 = "42"
    and var2 = "128"
    and var3 = "8956"
    and var4 = "256"
    and var5 = "20456"
    and var6 = "10243"
    in let bigint1a = Bigint.Bigint.bigint_of_string var1
       and bigint2a = Bigint.Bigint.bigint_of_string var2
       and bigint3a = Bigint.Bigint.bigint_of_string var3
       and bigint4a = Bigint.Bigint.bigint_of_string var4
       and bigint5a = Bigint.Bigint.bigint_of_string var5
       and bigint6a = Bigint.Bigint.bigint_of_string var6
       and bigint1b = Bigint.Bigint.bigint_of_string var1
       and bigint2b = Bigint.Bigint.bigint_of_string var2
       and bigint3b = Bigint.Bigint.bigint_of_string var3
       and bigint4b = Bigint.Bigint.bigint_of_string var4
       and bigint5b = Bigint.Bigint.bigint_of_string var5
       and bigint6b = Bigint.Bigint.bigint_of_string var6
       and bigint1c = Bigint.Bigint.bigint_of_string var1
       and bigint2c = Bigint.Bigint.bigint_of_string var2
       and bigint3c = Bigint.Bigint.bigint_of_string var3
       and bigint4c = Bigint.Bigint.bigint_of_string var4
       and bigint5c = Bigint.Bigint.bigint_of_string var5
       and bigint6c = Bigint.Bigint.bigint_of_string var6
       in let expr1 = Arithexpr.ArithExpr.sub(Arithexpr.ArithExpr.mul(Arithexpr.ArithExpr.modulo(Arithexpr.ArithExpr.value(bigint2a), Arithexpr.ArithExpr.value(bigint1a)), Arithexpr.ArithExpr.value(bigint3a)), Arithexpr.ArithExpr.div(Arithexpr.ArithExpr.add(Arithexpr.ArithExpr.value(bigint4a), Arithexpr.ArithExpr.value(bigint5a)), Arithexpr.ArithExpr.value(bigint6a)))
	  and expr2 = Arithexpr.ArithExpr.mul(Arithexpr.ArithExpr.mul(Arithexpr.ArithExpr.value(bigint1b), Arithexpr.ArithExpr.sub(Arithexpr.ArithExpr.value(bigint3b), Arithexpr.ArithExpr.value(bigint2b))), Arithexpr.ArithExpr.div(Arithexpr.ArithExpr.value(bigint5b), Arithexpr.ArithExpr.add(Arithexpr.ArithExpr.value(bigint6b), Arithexpr.ArithExpr.value(bigint4b))))
	  and expr3 = Arithexpr.ArithExpr.add(Arithexpr.ArithExpr.div(Arithexpr.ArithExpr.add(Arithexpr.ArithExpr.value(bigint1c), Arithexpr.ArithExpr.value(bigint4c)), Arithexpr.ArithExpr.value(bigint2c)), Arithexpr.ArithExpr.sub(Arithexpr.ArithExpr.value(bigint5c), Arithexpr.ArithExpr.mul(Arithexpr.ArithExpr.value(bigint3c), Arithexpr.ArithExpr.value(bigint6c))))
	  in let ret1 = Bigint.Bigint.string_of_bigint (Arithexpr.ArithExpr.solve_arith_expr expr1)
	     and ret2 = Bigint.Bigint.string_of_bigint (Arithexpr.ArithExpr.solve_arith_expr expr2)
	     and ret3 = Bigint.Bigint.string_of_bigint (Arithexpr.ArithExpr.solve_arith_expr expr3)
	     and str1 = Arithexpr.ArithExpr.string_of_arith_expr expr1
	     and str2 = Arithexpr.ArithExpr.string_of_arith_expr expr2
	     and str3 = Arithexpr.ArithExpr.string_of_arith_expr expr3
	     in begin
	       print_endline "###################################";
	       print_endline "##### Test of ArithExpr modul #####";
	       print_endline "###################################";
	       print_endline "expr1 = sub( mul( modulo(128, 42), 8956), div( add(256, 20456), 10243))";
	       print_endline "expr2 = mul( mul(42, sub(8956, 128)), div(20456, add(10243, 256)))";
	       print_endline "expr3 = add( div( add(42, 256), 128), sub(20456, mul(8956, 10243)))";
	       print_endline "## Test of string_of_arith_expr ##";
	       print_endline ("string_of_arith_expr expr1 = "^str1^" [OK]");
	       print_endline ("string_of_arith_expr expr2 = "^str2^" [OK]");
	       print_endline ("string_of_arith_expr expr3 = "^str3^" [OK]");
	       print_endline "## Test of solve_arith_expr ##";
	       print_string ("solve_arith_expr expr1 = "^ret1^" ");
	       if ret1 = "17910" then print_endline "[OK]" else print_endline "[FAIL]";
	       print_string ("solve_arith_expr expr2 = "^ret2^" ");
	       if ret2 = "370776" then print_endline "[OK]" else print_endline "[FAIL]";
	       print_string ("solve_arith_expr expr3 = "^ret3^" ");
	       if ret3 = "-91715850" then print_endline "[OK]" else print_endline "[FAIL]"
	     end

end
