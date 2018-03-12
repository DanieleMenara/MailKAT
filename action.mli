open Core.Std
open Test

type action =
  | Mod of (string * string)
  | Red of string
  | Store of string
  | Test of Test.test
  | Seq of (action * action)
  | Sum of (action * action)
  | Star of action

val to_string: action -> string

val is_sum : action -> bool

val is_conj_clause : action -> bool

val to_disj_normal_form : action -> action
