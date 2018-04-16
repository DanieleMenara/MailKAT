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

(** Exception thrown when a conj clause has more than one test on the "mta" field *)
exception Multiple_mtas;;

(** Convert to string for pretty printing *)
val to_string: fmla -> string

(** Make sum of fmlae *)
val mk_sum: fmla -> fmla -> fmla

(** Make sequential composition of fmlae *)
val mk_seq: fmla -> fmla -> fmla

(** Checks that the fmla does not contain any star operator,
    modifications, or tests on the "mailbox" header.
    Note: see report on justification *)
val is_convertible_to_snf: fmla -> bool

(** Convert fmla to SNF. Assume it is convertible to SNF *)
val to_snf: fmla -> fmla

(** Check on which mta the fmla tests on. If >1 it throws Multiple_mtas exception.
    Assume it is a conjunctive clause *)
val which_mta: fmla -> string option

(** Remove tests on "mta" header. These cannot be converted to Sieve syntax for example *)
val remove_mta_tests: fmla -> fmla

(** Compiles fmla to Sieve. Assume it is convertible to SNF. *)
val to_sieve: fmla -> string
