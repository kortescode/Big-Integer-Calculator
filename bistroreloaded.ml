(*******************************************)
(*                                         *)
(*          Module BistroReloaded          *)
(*                                         *)
(*******************************************)

module type BISTRORELOADED =
sig

  exception AlreadyDefined of string
  exception BadBaseVal of int
  exception Eof

  val init : unit -> unit
  val launch : unit -> unit

  val front_end : unit -> unit

end

module BistroReloaded : BISTRORELOADED =
struct

  (*******************************)
  (*          Exception          *)
  (*******************************)

  exception AlreadyDefined of string
  exception BadBaseVal of int
  exception Eof

  (**************************)
  (*          Argv          *)
  (**************************)

  let def_fd = ref false
  let def_ifile = ref false
  let def_obase = ref false
  let fd = ref Unix.stdin
  let ifile = ref ""
  let obase = ref 10

  (************************************)
  (*          Util functions          *)
  (************************************)

  let get_obase () =
    match !obase with
      | 2  -> Bigint.Bigint.binary
      | 8  -> Bigint.Bigint.octal
      | 16 -> Bigint.Bigint.hexadecimal
      | _  -> Bigint.Bigint.decimal

  let rec get_fd () =
    if !def_ifile && not !def_fd then begin fd := (Unix.openfile !ifile [Unix.O_RDONLY] 0o440); def_fd := true; get_fd () end
    else !fd

  let close_fd () =
    if !def_fd then Unix.close !fd

  let rec is_empty str ndx =
    if ndx == String.length str then true
    else if str.[ndx] != ' ' && str.[ndx] != '\t' && str.[ndx] != '\n' then false
    else is_empty str (ndx + 1)

  (**************************************)
  (*          Launch functions          *)
  (**************************************)

  let get_lexbuf () =
    let rec read_line str buff =
      let ret = Unix.read (get_fd ()) buff 0 1
      in if (String.length str) == 0 && ret == 0 then raise (Eof)
	else if (ret == 0 || buff.[0] == '\n') && not (is_empty str 0) then (str^"\n")
	else if (ret == 0 || buff.[0] == '\n') && (is_empty str 0) then read_line "" " "
	else read_line (str^buff) " "
    in Lexing.from_string (read_line "" " ")

  let rec launch () =
    try
      let expr = Parser.main Lexer.token (get_lexbuf ())
      in let result = Arithexpr.ArithExpr.solve_arith_expr expr
	 in let str = Bigint.Bigint.string_of_bigint_base (get_obase ()) result
	    in begin print_endline ("= "^str^"\n"); launch (); close_fd () end
    with
      | Parser.Error              -> begin print_endline "error: parsing: bad syntax\n"; launch () end
      | Failure	str               -> begin print_endline ("error: "^str^"\n"); launch () end
      | Bigint.Bigint.OpByZero op -> begin print_endline ("error: bistro: can't "^op^" by zero\n"); launch () end

  (************************************)
  (*          Init functions          *)
  (************************************)

  let set_ifile file =
    if !def_ifile then raise (AlreadyDefined "input file")
    else begin ifile := file; def_ifile := true end

  let set_obase base =
      if !def_obase then raise (AlreadyDefined "output base")
      else if base == 2 || base == 8 || base == 10 || base == 16 then begin obase := base; def_obase := true end
	   else raise (BadBaseVal base)

  let init () =
    begin
      print_endline "";
      Sys.set_signal Sys.sigint Sys.Signal_ignore;
      Arg.parse [ ("-obase", Arg.Int(set_obase), "Conversion base for output numbers") ] (set_ifile) "usage: ./bistro [-obase (2|8|10|16)] [inputfile]"
    end

  (****************************************)
  (*          Front-end function          *)
  (****************************************)

  let front_end () =
    try
      begin init (); launch () end
    with
      | AlreadyDefined obj               -> print_endline ("error: bistro: "^obj^" is already defined\n")
      | BadBaseVal nbr                   -> begin print_string "error: bistro: bad out base value "; print_int nbr; print_endline "\n" end
      | Unix.Unix_error (err, fct, file) -> print_endline ("error: unix: can't perform "^fct^" on file \""^file^"\"\n")
      | Eof                              -> ()
      | _                                -> print_endline "error: bistro: unknown error\n"

end
