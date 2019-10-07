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

module BistroReloaded : BISTRORELOADED
