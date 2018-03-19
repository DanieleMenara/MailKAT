type (+'a, +'b) seq = [ `Seq of 'a * 'b ]
type (+'a, +'b) sum = [ `Sum of 'a * 'b ]

type test =
  [ `Const of bool
  | `Test of (string * string) ]

type +'a neg = [ `Not of 'a ] constraint 'a = [< test | ('a,'a) seq | ('a,'a) sum | `Not of 'a ]

type tests =
  [ test
  | (tests, tests) seq
  | (tests, tests) sum
  | tests neg ]

val is_drop: tests -> bool

val mk_const: bool -> test

val to_string: tests -> string

val is_equal: tests -> tests -> bool

val test_on: tests -> string -> string -> bool

val eval_on: tests -> string -> string -> bool

val to_disj_normal_form: tests -> tests

val to_sieve: tests -> string
