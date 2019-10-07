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

module Bigint : BIGINT
