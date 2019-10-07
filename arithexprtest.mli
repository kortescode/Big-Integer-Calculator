(******************************************)
(*                                        *)
(*          Module ArithExprTest          *)
(*                                        *)
(******************************************)

module type ARITHEXPRTEST =
sig

  val launch_tests : unit -> unit

end

module ArithExprTest : ARITHEXPRTEST
