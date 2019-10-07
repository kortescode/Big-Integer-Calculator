(***********************************)
(*                                 *)
(*          Module Bigint          *)
(*                                 *)
(***********************************)

module type BIGINT =
sig

  exception OpByZero of string

  type bigint
  type base

  val binary : base
  val octal : base
  val decimal : base
  val hexadecimal : base

  val bigint_of_string : string -> bigint

  val string_of_bigint : bigint -> string
  val string_of_bigint_base : base -> bigint -> string

  val add : bigint -> bigint -> bigint
  val sub : bigint -> bigint -> bigint
  val mul : bigint -> bigint -> bigint
  val div : bigint -> bigint -> bigint
  val modulo : bigint -> bigint -> bigint

end

module Bigint : BIGINT =
struct

  (*******************************)
  (*          Exception          *)
  (*******************************)

  exception OpByZero of string

  (***************************)
  (*          Types          *)
  (***************************)

  type bigint = (bool * int array)

  type base = Binary | Octal | Decimal | Hexadecimal

  let binary = Binary
  let octal = Octal
  let decimal = Decimal
  let hexadecimal = Hexadecimal

  (************************************)
  (*          Util functions          *)
  (************************************)

  let rec addi (neg, bigint) nbr ndx ret =
    match ndx with
      | (-1) -> ()
      | _    -> let tmp = bigint.(ndx) + nbr + ret
		in begin bigint.(ndx) <- tmp mod 10; addi (neg, bigint) 0 (ndx - 1) (tmp / 10) end

  let rec mult (neg, bigint) nbr ndx ret =
    match ndx with
      | (-1) -> ()
      | _    -> let tmp = bigint.(ndx) * nbr + ret
		in begin bigint.(ndx) <- tmp mod 10; mult (neg, bigint) nbr (ndx - 1) (tmp / 10) end

  let rec divi (neg, bigint) nbr ndx ret =
    if ndx == Array.length bigint then ()
    else let tmp = ((float_of_int(bigint.(ndx)) /. float_of_int(nbr))) +. ret
	 in begin
	   bigint.(ndx) <- int_of_float(tmp);
	   if bigint.(ndx) >= 10 then begin bigint.(ndx - 1) <- bigint.(ndx - 1) + (bigint.(ndx) / 10); bigint.(ndx) <- bigint.(ndx) mod 10 end;
	   divi (neg, bigint) nbr (ndx + 1) (mod_float(tmp *. 10.0) 10.0)
	 end

  let modul (neg, bigint) nbr =
    let rec modul_calc (ret_neg, bigint_nbr) adding =
      if bigint = bigint_nbr then adding
      else begin addi (ret_neg, bigint_nbr) 1 (Array.length bigint_nbr - 1) 0; modul_calc (ret_neg, bigint_nbr) (adding + 1) end
    in let (ret_neg, ret_mul) = (neg, Array.copy bigint)
       in begin divi (ret_neg, ret_mul) nbr 0 0.0; mult (ret_neg, ret_mul) nbr (Array.length ret_mul - 1) 0; modul_calc (ret_neg, ret_mul) 0 end

  let is_sup (neg1, bigint1) (neg2, bigint2) =
    let rec rev (neg, bigint) =
      (neg, (Array.of_list (List.rev (Array.to_list bigint))))
    and is_sup_calc (neg1, bigint1) (neg2, bigint2) ndx =
      if ndx < 0 then false
      else let int1 = if ndx >= (Array.length bigint1) then 0 else bigint1.(ndx)
	   and int2 = if ndx >= (Array.length bigint2) then 0 else bigint2.(ndx)
	   in if int1 > int2 then true
	      else if int1 < int2 then false
	      else is_sup_calc (neg1, bigint1) (neg2, bigint2) (ndx - 1)
    in let tmp_ndx = if (Array.length bigint1) > (Array.length bigint2) || (Array.length bigint1) == (Array.length bigint2) then (Array.length bigint1 - 1)
		     else (Array.length bigint2 - 1)
       in is_sup_calc (rev (neg1, bigint1)) (rev (neg2, bigint2)) tmp_ndx

  let rec is_null bigint ndx =
    if ndx == Array.length bigint then true
    else if bigint.(ndx) != 0 then false
	 else is_null bigint (ndx + 1)

  let rec epur_str str ret neg ndx start =
    if ndx == String.length str then if start then "0"
				     else if neg then (Char.escaped '-')^ret
				     else ret
    else if start && str.[ndx] == '0' then epur_str str ret neg (ndx + 1) start
    else epur_str str (ret^(Char.escaped str.[ndx])) neg (ndx + 1) false

  (***************************************)
  (*          Convert functions          *)
  (***************************************)

  let rec bigint_of_string_base str (neg, bigint) ndx base =
    if ndx == (String.length str) then (neg, bigint)
    else let tmp =
	   if (Char.code str.[ndx]) - (Char.code '0') > 9 then (Char.code str.[ndx]) - (Char.code '0') - 39
	   else (Char.code str.[ndx]) - (Char.code '0')
	 in begin mult (neg, bigint) base (Array.length bigint - 1) 0; addi (neg, bigint) tmp (Array.length bigint - 1) 0; bigint_of_string_base str (neg, bigint) (ndx + 1) base end

  let bigint_of_string str =
    match str with
      | str when String.length str >= 3 &&
	  str.[0] == '0' && str.[1] == 'x' -> let nbr = String.lowercase (String.sub str 2 (String.length str - 2))
					      in bigint_of_string_base nbr (false, (Array.make ((String.length nbr) + ((String.length nbr) / 5) + 1) 0)) 0 16
      | str when String.length str >= 3 &&
	  str.[0] == '0' && str.[1] == 'b' -> let nbr = String.sub str 2 (String.length str - 2)
					      in bigint_of_string_base nbr (false, (Array.make (String.length str) 0)) 0 2
      | str when String.length str >= 2 &&
	  str.[0] == '0'		   -> let nbr = String.sub str 1 (String.length str - 1)
					      in bigint_of_string_base nbr (false, (Array.make (String.length str) 0)) 0 8
      | _				   -> bigint_of_string_base str (false, (Array.make (String.length str) 0)) 0 10

  let string_of_bigint_base base (neg, bigint) =
    let rec string_of_bigint_base_create str base ndx =
      if ndx < 0 then epur_str str "" neg 0 true
      else let char =
	     if (modul (neg, bigint) base) < 10 then Char.chr((modul (neg, bigint) base) + (Char.code '0'))
	     else Char.chr((modul (neg, bigint) base) + 55)
	   in begin Bytes.set str ndx char; divi (neg, bigint) base 0 0.0; string_of_bigint_base_create str base (ndx - 1) end
    in match base with
      | Binary	    -> string_of_bigint_base_create (Bytes.create (Array.length bigint * 4)) 2 ((Array.length bigint * 4) - 1)
      | Octal	    -> string_of_bigint_base_create (Bytes.create (Array.length bigint * 2)) 8 ((Array.length bigint * 2) - 1)
      | Decimal	    -> string_of_bigint_base_create (Bytes.create (Array.length bigint)) 10 ((Array.length bigint) - 1)
      | Hexadecimal -> string_of_bigint_base_create (Bytes.create (Array.length bigint)) 16 ((Array.length bigint) - 1)

  let string_of_bigint (neg, bigint) =
    string_of_bigint_base Decimal (neg, bigint)

  (**************************************)
  (*          Calcul functions          *)
  (**************************************)

  let rec add (neg1, bigint1) (neg2, bigint2) =
    let rec add_calc (neg, bigint) ndx ndx1 ndx2 ret =
      if ndx < 0 then (neg, bigint)
      else let int1 = if ndx1 < 0 then 0 else bigint1.(ndx1)
	   and int2 = if ndx2 < 0 then 0 else bigint2.(ndx2)
	   in begin bigint.(ndx) <- (int1 + int2 + ret) mod 10; add_calc (neg, bigint) (ndx - 1) (ndx1 - 1) (ndx2 - 1) ((int1 + int2 + ret) / 10) end
    in let size1 = Array.length bigint1
       and size2 = Array.length bigint2
       in if neg1 && neg2 then add_calc (true, (Array.make (size1 + size2) 0)) (size1 + size2 - 1) (size1 - 1) (size2 - 1) 0
	 else if not neg1 && neg2 then sub (neg1, bigint1) (false, bigint2)
	 else if neg1 && not neg2 then sub (neg2, bigint2) (false, bigint1)
	 else add_calc (false, (Array.make (size1 + size2) 0)) (size1 + size2 - 1) (size1 - 1) (size2 - 1) 0

  and sub (neg1, bigint1) (neg2, bigint2) =
    let rec sub_calc (neg, bigint) (neg1, bigint1) (neg2, bigint2) ndx ndx1 ndx2 ret =
      if ndx < 0 then (neg, bigint)
      else let int1 = if ndx1 < 0 then 0 else bigint1.(ndx1)
	   and int2 = if ndx2 < 0 then 0 else bigint2.(ndx2)
	   in let tmp = if int1 - int2 < 0 then 10 else 0
	      in begin bigint.(ndx) <- (int1 + tmp) - (int2 + ret); sub_calc (neg, bigint) (neg1, bigint1) (neg2, bigint2) (ndx - 1) (ndx1 - 1) (ndx2 - 1) (tmp / 10) end
    in let size1 = Array.length bigint1
       and size2 = Array.length bigint2
       in if neg1 && neg2 then if is_sup (neg1, bigint1) (neg2, bigint2) then sub_calc (true, (Array.make (size1 + size2) 0)) (neg1, bigint1) (neg2, bigint2) (size1 + size2 - 1) (size1 - 1) (size2 - 1) 0
			       else sub_calc (false, (Array.make (size1 + size2) 0)) (neg2, bigint2) (neg1, bigint1) (size1 + size2 - 1) (size2 - 1) (size1 - 1) 0
	 else if not neg1 && neg2 then add (neg1, bigint1) (false, bigint2)
	 else if neg1 && not neg2 then add (neg1, bigint1) (true, bigint2)
	 else if is_sup (neg1, bigint1) (neg2, bigint2) then sub_calc (false, (Array.make (size1 + size2) 0)) (neg1, bigint1) (neg2, bigint2) (size1 + size2 - 1) (size1 - 1) (size2 - 1) 0
	 else sub_calc (true, (Array.make (size1 + size2) 0)) (neg2, bigint2) (neg1, bigint1) (size1 + size2 - 1) (size2 - 1) (size1 - 1) 0

  let mul (neg1, bigint1) (neg2, bigint2) =
    let rec mul_calc (neg, bigint) ndx1 ndx2 ret =
      if ndx2 < 0 then (false, bigint)
      else if ndx1 < 0 then let tmp = bigint.(ndx1 + ndx2 + 1) + ret
			    in begin if tmp >= 10 then bigint.(ndx1 + ndx2) <- bigint.(ndx1 + ndx2) + (tmp / 10); bigint.(ndx1 + ndx2 + 1) <- (tmp mod 10); mul_calc (neg, bigint) (Array.length bigint1 - 1) (ndx2 - 1) 0 end
      else let tmp = bigint.(ndx1 + ndx2 + 1) + (bigint1.(ndx1) * bigint2.(ndx2)) + ret
	   in begin bigint.(ndx1 + ndx2 + 1) <- (tmp mod 10); mul_calc (neg, bigint) (ndx1 - 1) ndx2 (tmp / 10) end
    in if (neg1 && neg2) || (not neg1 && not neg2) then mul_calc (false, (Array.make ((Array.length bigint1) + (Array.length bigint2)) 0)) (Array.length bigint1 - 1) (Array.length bigint2 - 1) 0
      else mul_calc (true, (Array.make ((Array.length bigint1) + (Array.length bigint2)) 0)) (Array.length bigint1 - 1) (Array.length bigint2 - 1) 0

  let div (neg1, bigint1) (neg2, bigint2) =
    let rec div_calc (neg, bigint) =
      if is_sup (mul (neg, bigint) (neg2, bigint2)) (neg1, bigint1) then sub (neg, bigint) (false, [|1|])
      else div_calc (add (neg, bigint) (false, [|1|]))
    in if (is_null bigint2 0) then raise (OpByZero "divide")
      else if (neg1 && neg2) || (not neg1 && not neg2) then div_calc (false, [|0|])
      else div_calc (true, [|0|])

  let modulo (neg1, bigint1) (neg2, bigint2) =
    let rec modulo_calc (neg, bigint) (neg_div, bigint_div) =
      if is_sup (neg_div, bigint_div) (neg1, bigint1) then sub (neg, bigint) (false, [|1|])
      else modulo_calc (add (neg, bigint) (false, [|1|])) (add (neg_div, bigint_div) (false, [|1|]))
    in if (is_null bigint2 0) then raise (OpByZero "modulo")
      else if neg1 then modulo_calc (true, [|0|]) (mul (div (neg1, bigint1) (neg2, bigint2)) (neg2, bigint2))
      else modulo_calc (false, [|0|]) (mul (div (neg1, bigint1) (neg2, bigint2)) (neg2, bigint2))

end
