open Test

(** Star operator *)
type +'a star = [ `Star of 'a ]

(** Action term *)
type action =
  [ `Mod of (string * string)
  | `Red of string
  | `Store of string ]

(** MailKAT formula *)
type fmla =
  [ action
  | (fmla, fmla) seq
  | (fmla, fmla) sum
  | fmla star
  | test
  | tests neg ]

exception Multiple_mtas;;

val to_string: fmla -> string

val mk_sum: fmla -> fmla -> fmla

val mk_seq: fmla -> fmla -> fmla

val to_snf: fmla -> fmla

val contains_star: fmla -> bool

val which_mta: fmla -> string option

val remove_mta_tests: fmla -> fmla

val to_sieve: fmla -> string
