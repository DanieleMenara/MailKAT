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

let rec to_string a =
  match a with
    | Mod(f,v)      -> Printf.sprintf "%s <- %s" f v
    | Red(a)        -> Printf.sprintf "red[%s]" a
    | Store(m)      -> Printf.sprintf "store[%s]" m
    | Test t        -> Printf.sprintf "%s" (Test.to_string t)
    | Seq (a1, a2)  -> Printf.sprintf "(%s ; %s)" (to_string a1) (to_string a2)
    | Sum (a1, a2)  -> Printf.sprintf "(%s + %s)" (to_string a1) (to_string a2)
    | Star (a)      -> Printf.sprintf "(%s)*" (to_string a)

let is_sum fmla =
  match fmla with
    | Sum (_,_) -> true
    | _         -> false

let rec is_conj_clause fmla =
  match fmla with
    | Sum(p, q) -> false
    | Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | Star p    -> is_conj_clause p
    | _         -> true

let rec to_disj_normal_form fmla =
  match fmla with
    | Seq(Sum (p, q), r) -> Sum(to_disj_normal_form (Seq(p, r)), to_disj_normal_form (Seq(q, r)))
    | Seq(p, Sum(q, r))  -> Sum(to_disj_normal_form (Seq(p, q)), to_disj_normal_form (Seq(p, r)))
    | Seq(p, q)          -> if is_conj_clause p && is_conj_clause q then Seq(p, q)
                            else to_disj_normal_form (Seq(to_disj_normal_form p, to_disj_normal_form q))
    | Sum(p, q)          -> Sum(to_disj_normal_form p, to_disj_normal_form q)
    | Star(p)            -> Star(to_disj_normal_form p)
    | _                  -> fmla
