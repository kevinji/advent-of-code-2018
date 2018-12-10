open! Core
open! Async

module Make_counter (Key : Comparable.S) : sig
  val of_list : Key.t list -> int Key.Map.t
end

val exec_re : 'a Tyre.re -> string -> 'a
