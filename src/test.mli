(** Test terms. Note: None is used to represent null*)
type test =
  [ `Const of bool
  | `Test of (string * string option) ]

(** Composition/conjunction *)
type (+'a, +'b) seq = [ `Seq of 'a * 'b ]
(** Union/disjunction *)
type (+'a, +'b) sum = [ `Sum of 'a * 'b ]

(** Negation of test(s). Using polymorphic variant, negation is restricted to apply only to tests
    as per MailKAT syntax *)
type +'a neg = [ `Not of 'a ] constraint 'a = [< test | ('a,'a) seq | ('a,'a) sum | `Not of 'a ]

(** Tests: terms above define bases cases for inductive definition using
    seq, sum and neg. Refer to MailKAT syntax. *)
type tests =
  [ test
  | (tests, tests) seq
  | (tests, tests) sum
  | tests neg ]

(* Check whether a conjunctive clause contains 0 *)
val is_drop: tests -> bool

(** Make test from boolean where true => 1 and false => 0 *)
val mk_const: bool -> test

(** Make test *)
val mk_test: string -> string option -> tests

(** Convert to string for pretty printing *)
val to_string: tests -> string

(** Check whther 2 tsts are syntactically equivalent - i.e. not semantic equivalence *)
val is_equal: tests -> tests -> bool

(** Where the tests in the first argument contain a test on the header provided in the second
    argument *)
val test_on: tests -> string -> bool

(** check whether the tests in the first argument are inconsistent with the test in the
    second argument, e.g. if second argument is f=n, then first argument should not contain
    f = m or f != n *)
val contra: tests -> tests -> bool

(* Convert tests to SNF: negations are driven down using De Morgan's laws
and disjunction above all conjunctions *)
val to_snf: tests -> tests

(* Simplify to 0 conjunction of tests that contain 0 or (f = n) ; (f = m) or (f = n) ; !(f = n) *)
val simplify: tests -> tests

(* Remove tests on "mta". These cannot be converted to Sieve syntax for example *)
val remove_mta: tests -> tests

(** Compilation of tests to Sieve as outlines in report, Appendix B.1.2
    (tests need to be in SNF) *)
val to_sieve: tests -> string
