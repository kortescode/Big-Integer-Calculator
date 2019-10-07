(***************************************)
(*                                     *)
(*          Module BigintTest          *)
(*                                     *)
(***************************************)

module type BIGINTTEST =
sig

  val launch_tests : unit -> unit

end

module BigintTest : BIGINTTEST =
struct

  let launch_tests () =
    let var1 = "42424"
    and var2 = "0b101101"
    and var3 = "0xA34f3B2"
    and var4 = "034572"
    in let bigint1b = Bigint.Bigint.bigint_of_string var1
       and bigint2b = Bigint.Bigint.bigint_of_string var2
       and bigint3b = Bigint.Bigint.bigint_of_string var3
       and bigint4b = Bigint.Bigint.bigint_of_string var4
       and bigint1c = Bigint.Bigint.bigint_of_string var1
       and bigint2c = Bigint.Bigint.bigint_of_string var2
       and bigint3c = Bigint.Bigint.bigint_of_string var3
       and bigint4c = Bigint.Bigint.bigint_of_string var4
       and bigint1d = Bigint.Bigint.bigint_of_string var1
       and bigint2d = Bigint.Bigint.bigint_of_string var2
       and bigint3d = Bigint.Bigint.bigint_of_string var3
       and bigint4d = Bigint.Bigint.bigint_of_string var4
       in let var1b = Bigint.Bigint.string_of_bigint bigint1b
	  and var2b = Bigint.Bigint.string_of_bigint bigint2b
          and var3b = Bigint.Bigint.string_of_bigint bigint3b
	  and var4b = Bigint.Bigint.string_of_bigint bigint4b
	  and var1c = Bigint.Bigint.string_of_bigint_base Bigint.Bigint.hexadecimal bigint1c
          and var2c = Bigint.Bigint.string_of_bigint_base Bigint.Bigint.decimal bigint2c
	  and var3c = Bigint.Bigint.string_of_bigint_base Bigint.Bigint.octal bigint3c
	  and var4c = Bigint.Bigint.string_of_bigint_base Bigint.Bigint.binary bigint4c
	  and add1 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.add bigint1d bigint2d)
	  and add2 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.add bigint3d bigint4d)
	  and sub1 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.sub bigint1d bigint2d)
	  and sub2 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.sub bigint3d bigint4d)
	  and mul1 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.mul bigint1d bigint2d)
	  and mul2 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.mul bigint3d bigint4d)
	  and div1 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.div bigint1d bigint2d)
	  and div2 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.div bigint3d bigint4d)
	  and mod1 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.modulo bigint1d bigint2d)
	  and mod2 = Bigint.Bigint.string_of_bigint (Bigint.Bigint.modulo bigint3d bigint4d)
	  in begin
	    print_endline "################################";
	    print_endline "##### Test of Bigint modul #####";
	    print_endline "################################";
	    print_endline "## Test of bigint_of_string ##";
	    print_endline ("bigint1 = bigint_of_string \""^var1^"\" [OK]");
	    print_endline ("bigint2 = bigint_of_string \""^var2^"\" [OK]");
	    print_endline ("bigint3 = bigint_of_string \""^var3^"\" [OK]");
	    print_endline ("bigint4 = bigint_of_string \""^var4^"\" [OK]");
	    print_endline "## Test of string_of_bigint ##";
	    print_string ("string_of_bigint bigint1 = "^var1b^" ");
	    if var1b = "42424" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("string_of_bigint bigint2 = "^var2b^" ");
	    if var2b = "45" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("string_of_bigint bigint3 = "^var3b^" ");
	    if var3b = "171242418" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("string_of_bigint bigint4 = "^var4b^" ");
	    if var4b = "14714" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_endline "## Test of string_of_bigint_base ##";
	    print_string ("string_of_bigint_base Hexadecimal bigint1 = "^var1c^" ");
	    if var1c = "A5B8" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("string_of_bigint_base Decimal bigint2 = "^var2c^" ");
	    if var2c = "45" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("string_of_bigint_base Octal bigint3 = "^var3c^" ");
	    if var3c = "1215171662" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("string_of_bigint_base Binary bigint4 = "^var4c^" ");
	    if var4c = "11100101111010" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_endline "## Test of add ##";
	    print_string ("add bigint1 bigint2 = "^add1^" ");
	    if add1 = "42469" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("add bigint3 bigint4 = "^add2^" ");
	    if add2 = "171257132" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_endline "## Test of sub ##";
	    print_string ("sub bigint1 bigint2 = "^sub1^" ");
	    if sub1 = "42379" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("sub bigint3 bigint4 = "^sub2^" ");
	    if sub2 = "171227704" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_endline "## Test of mul ##";
	    print_string ("mul bigint1 bigint2 = "^mul1^" ");
	    if mul1 = "1909080" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("mul bigint3 bigint4 = "^mul2^" ");
	    if mul2 = "2519660938452" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_endline "## Test of div ##";
	    print_string ("div bigint1 bigint2 = "^div1^" ");
	    if div1 = "942" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("div bigint3 bigint4 = "^div2^" ");
	    if div2 = "11638" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_endline "## Test of modulo ##";
	    print_string ("modulo bigint1 bigint2 = "^mod1^" ");
	    if mod1 = "34" then print_endline "[OK]" else print_endline "[FAIL]";
	    print_string ("modulo bigint3 bigint4 = "^mod2^" ");
	    if mod2 = "886" then print_endline "[OK]" else print_endline "[FAIL]";
	  end

end
