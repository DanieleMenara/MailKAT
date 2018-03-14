open Core.Std
open Test
open Util

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

let rec is_equal fmla1 fmla2 =
  match fmla1, fmla2 with
    | Sum(p1, q1), Sum(p2, q2) -> (is_equal p1 p2) && (is_equal q1 q2)
    | Seq(p1, q1), Seq(p2, q2) -> is_equal p1 p2 && is_equal q1 q2
    | Star(p1), Star(p2)       -> is_equal p1 p2
    | Mod(f1, v1), Mod(f2, v2) -> f1 = f2 && v1 = v2
    | Red(a1), Red(a2)         -> a1 = a2
    | Store(m1), Store(m2)     -> m1 = m2
    | Test(t1), Test(t2)       -> Test.is_equal t1 t2
    | _, _                     -> false

let is_sum fmla =
  match fmla with
    | Sum (_,_) -> true
    | Test (t)  -> Test.is_sum t
    | _         -> false

let is_test a =
  match a with
    | Test t -> true
    | _      -> false

let rec is_tests fmla =
  match fmla with
    | Sum(p, q) -> is_tests p && is_tests q
    | Seq(p, q) -> is_tests p && is_tests q
    | Star(p)   -> is_tests p
    | Test t    -> true
    | _         -> false

let rec to_test fmla =
  assert(is_tests fmla);
  let rec helper f =
    match f with
      | Sum(p, q) -> Test.mk_sum (helper p) (helper q)
      | Seq(p, q) -> Test.mk_seq (helper p) (helper q)
      | Test(t)   -> t
      | Star(t)   -> helper t
      | _         -> failwith "Fmla not convertible to test"
  in Test(helper fmla)

let is_simple_action a =
  match a with
    | Mod(f,v)      -> true
    | Red(a)        -> true
    | Store(m)      -> true
    | _             -> false

let rec is_actions fmla =
  match fmla with
    | Sum(p, q) -> is_actions p && is_actions q
    | Seq(p, q) -> is_actions p && is_actions q
    | Star(p)   -> is_actions p
    | p         -> is_simple_action p

let rec test_on tsts a =
  match tsts, a with
    | Sum(p, q), _         -> test_on p a || test_on q a
    | Seq(p, q), _         -> test_on p a || test_on q a
    | Star(p), _           -> test_on p a
    | Test t, Mod(f, v)    -> Test.test_on t f v
    | Test t, Red(a)       -> Test.test_on t "envrcpt" a
    | Test t, Store(m)     -> Test.test_on t "mailbox" m
    | _                    -> false

let rec eval_test_on fmla a =
  assert(is_tests fmla);
  match fmla, a with
    | Sum(p, q), _        -> eval_test_on p a || eval_test_on q a
    | Seq(p, q), _        -> eval_test_on p a && eval_test_on q a
    | Star(p), _          -> eval_test_on p a
    | Test(t), Mod(f, v)  -> Test.eval_on t f v
    | Test(t), Red(a)     -> Test.eval_on t "envrcpt" a
    | Test(t), Store(m)   -> Test.eval_on t "mailbox" m
    | _                   -> true

let rec is_conj_clause fmla =
  match fmla with
    | Sum(p, q) -> false
    | Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | Star p    -> is_conj_clause p
    | _         -> true

let to_disj_normal_form fmla =
  let rec helper fmla =
    match fmla with
      | Seq(Sum (p, q), r) -> Sum(helper (Seq(p, r)), helper (Seq(q, r)))
      | Seq(p, Sum(q, r))  -> Sum(helper (Seq(p, q)), helper (Seq(p, r)))
      | Seq(p, q)          -> Seq(helper p, helper q)
      | Sum(p, q)          -> Sum(helper p, helper q)
      | Star(p)            -> Star(helper p)
      | _                  -> fmla
  in Util.fixpoint is_equal helper fmla

let rec is_disj_normal_form fmla =
  match fmla with
    | Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | Sum(p, q) -> is_disj_normal_form p && is_disj_normal_form q
    | Star(p)   -> is_conj_clause p
    | _         -> true

(* Assume no tests on mailbox *)
let rec commute_actions fmla =
  let eval_single tsts p =
    if eval_test_on tsts p then Test(Test.Const(true)) else Test(Test.Const(false))
  in let commute_single f =
    match f with
      | Seq(p, q)  -> if (is_tests q) then (if not (test_on q p) then Seq(q, p) else Seq(eval_single q p, q)) else f
      | _          -> f
  in let commute_deep f =
    match f with
      | Seq(Seq(r, p), q)  -> if (is_tests q) then (if not (test_on q p) then Seq(Seq(r, q), p) else Seq(Seq(r, eval_single q p), q)) else f
      | _          -> f
  in match fmla with
    | Seq(Mod(f1, v1), q)         -> commute_single fmla
    | Seq(Red(a), q)              -> commute_single fmla
    | Seq(Store(m), q)            -> commute_single fmla
    | Seq(Seq(p, Mod(f1, v1)), q) -> commute_deep fmla
    | Seq(Seq(p, Red(a)), q)      -> commute_deep fmla
    | Seq(Seq(p, Store(m)), q)    -> commute_deep fmla
    | Seq(p, q)                   -> Seq(commute_actions p, commute_actions q)
    | _                           -> fmla


let simplify_drop fmla =
  assert(is_conj_clause fmla);
  let rec contains_drop f =
    match f with
      | Seq(p, q) -> contains_drop p || contains_drop q
      | Star(p)   -> contains_drop p
      | Test(t)   -> Test.is_drop t
      | _         -> false
  in if contains_drop fmla then Test(Test.Const(false)) else fmla

let rec simplify fmla =
  assert (is_disj_normal_form fmla);
  let rec assoc fmla =
    match fmla with
      | Sum(p, q)          -> Sum(assoc p, assoc q)
      | Seq(p, (Seq(q,r))) -> Seq(Seq(p, q), r)
      | Seq(p, q)          -> Seq(assoc p, assoc q)
      | Star(p)            -> Star(assoc p)
      | _                  -> fmla
  in match fmla with
    | Seq(p, q) -> simplify_drop (Util.fixpoint is_equal (Fn.compose commute_actions assoc) fmla)
    | Sum(p, q) -> Sum(simplify p, simplify q)
    | Star(p)   -> simplify p
    | _         -> fmla

(* Asume it is in assoc form *)
let rec split_tests_actions fmla =
  match fmla with
    | Sum(p, q)                      -> Sum(split_tests_actions p, split_tests_actions q)
    | Seq(Seq(p, q), r)              -> if is_actions r && is_simple_action q then split_tests_actions (Seq(p, (Seq(q, r)))) else fmla
    | Seq(p, q)                      -> Seq(p, q)
    | Star(p)                        -> Star(split_tests_actions p)
    | _                              -> fmla

let to_sieve fmla =
  let rec actions_to_sieve acts =
    match acts with
    | Mod(f, v)     -> failwith "Mod is not supported in Sieve translation"
    | Red(a)        -> assert (Util.is_address a); Printf.sprintf "redirect \"%s\";" a
    | Store(m)      -> Printf.sprintf "fileinto \"%s\";" m
    | Seq(a1, a2)   -> Printf.sprintf "%s\n%s\n" (actions_to_sieve a1) (actions_to_sieve a2)
  in let rec tests_to_sieve tsts =
    assert (is_tests tsts);
    match tsts with
      | Seq(t1, t2)  -> Printf.sprintf "%s, %s" (tests_to_sieve t1) (tests_to_sieve t2)
      | Test(t)      -> Test.to_sieve t
  (* TODO: add assert its in right form, no star *)
  in let rec helper p =
    match p with
      | Seq(tsts, actions) -> Printf.sprintf "if allof(%s) {\n %s \n}" (tests_to_sieve tsts) (actions_to_sieve actions)
      | Sum(p1, p2)        -> Printf.sprintf "%s\n %s\n" (helper p1) (helper p2)
      | _                  -> ""
  in helper (split_tests_actions (simplify (to_disj_normal_form fmla)))
