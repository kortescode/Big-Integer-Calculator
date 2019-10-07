(***************************************)
(*                                     *)
(*          Module BigintTest          *)
(*                                     *)
(***************************************)

module type BIGINTTEST =
sig

  val launch_tests : unit -> unit

end

module BigintTest : BIGINTTEST
