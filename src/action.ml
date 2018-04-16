open Core
open Test
open Util

type +'a star = [ `Star of 'a ]

type action =
  [ `Mod of (string * string)
  | `Red of string
  | `Store of string ]

type fmla =
  [ action
  | (fmla, fmla) seq
  | (fmla, fmla) sum
  | fmla star
  | test
  | tests neg ]

let rec to_string (a: fmla) =
  match a with
    | `Mod(f,v)                   -> Printf.sprintf "%s <- %s" f v
    | `Red(a)                     -> Printf.sprintf "Red[%s]" a
    | `Store(m)                   -> Printf.sprintf "Store[%s]" m
    | `Seq(a1, a2)                -> Printf.sprintf "(%s ; %s)" (to_string a1) (to_string a2)
    | `Sum (a1, a2)               -> Printf.sprintf "(%s + %s)" (to_string a1) (to_string a2)
    | `Star (a)                   -> Printf.sprintf "(%s)*" (to_string a)
    | (#test | `Not #tests) as t  -> Printf.sprintf "%s" (Test.to_string t)

let rec is_equal fmla1 fmla2 =
  match fmla1, fmla2 with
    | `Sum(p1, q1), `Sum(p2, q2)                                   -> (is_equal p1 p2) && (is_equal q1 q2)
    | `Seq(p1, q1), `Seq(p2, q2)                                   -> is_equal p1 p2 && is_equal q1 q2
    | `Star(p1), `Star(p2)                                         -> is_equal p1 p2
    | `Mod(f1, v1), `Mod(f2, v2)                                   -> f1 = f2 && v1 = v2
    | `Red(a1), `Red(a2)                                           -> a1 = a2
    | `Store(m1), `Store(m2)                                       -> m1 = m2
    | ((#test | `Not #tests) as t1), ((#test | `Not #tests) as t2) -> Test.is_equal t1 t2
    | _, _                                                         -> false

let rec is_sum fmla =
  match fmla with
    | `Sum (_,_)         -> true
    | `Not (#tests as t) -> is_sum t
    | _                  -> false

let mk_sum a1 a2 = `Sum(a1, a2)

let mk_seq a1 a2 = `Seq(a1, a2)

let rec is_tests fmla =
  match fmla with
    | `Sum(p, q)            -> is_tests p && is_tests q
    | `Seq(p, q)            -> is_tests p && is_tests q
    | `Star(p)              -> is_tests p
    | (#test | `Not #tests) -> true
    | _                     -> false

let rec is_actions fmla =
  match fmla with
    | `Sum(p, q) -> is_actions p && is_actions q
    | `Seq(p, q) -> is_actions p && is_actions q
    | `Star(p)   -> is_actions p
    | #action    -> true
    | _          -> false

let rec is_conj_clause fmla =
  match fmla with
    | `Sum(p, q) -> false
    | `Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | `Star p    -> is_conj_clause p
    | _          -> true

let rec contains_star fmla =
  match fmla with
    | `Sum(p, q)                      -> contains_star p || contains_star q
    | `Seq(p, q)                      -> contains_star p || contains_star q
    | `Star p                         -> true
    | (#test | `Not #tests | #action) -> false

let rec contains_mod fmla =
  match fmla with
    | `Sum(p, q)                      -> contains_mod p || contains_mod q
    | `Seq(p, q)                      -> contains_mod p || contains_mod q
    | `Star p                         -> contains_mod p
    | `Mod(_, _)                      -> true
    | (#test | `Not #tests | #action) -> false

let rec test_on fmla h =
  match fmla with
    | `Sum(p, q)                 -> contains_mod p || contains_mod q
    | `Seq(p, q)                 -> contains_mod p || contains_mod q
    | `Star p                    -> contains_mod p
    | (#test | `Not #tests) as t -> Test.test_on t h
    | #action                    -> false

let assoc fmla =
  let rec helper f =
    match f with
      | `Sum(p, q)         -> `Sum(helper p, helper q)
      | `Seq(`Seq(p,q), r) -> `Seq(p, helper (`Seq(q, r)))
      | `Seq(p, q)         -> `Seq(helper p, helper q)
      | `Star(p)           -> `Star(helper p)
      | _                  -> f
  in Util.fixpoint is_equal helper fmla

(* Apply distributive axioms until union/disjunciton is above all composition/conjunction *)
let to_disjunctive_normal_form fmla =
  let rec helper fmla =
    match fmla with
      | `Seq(`Sum (p, q), r)        -> `Sum(helper (`Seq(p, r)), helper (`Seq(q, r)))
      | `Seq(p, `Sum(q, r))         -> `Sum(helper (`Seq(p, q)), helper (`Seq(p, r)))
      | `Seq(p, q)                  -> `Seq(helper p, helper q)
      | `Sum(p, q)                  -> `Sum(helper p, helper q)
      | `Star(p)                    -> `Star(helper p)
      | (#test | `Not #tests) as t  -> ((Test.to_snf t) :> fmla)
      | #action as a                -> a
  in Util.fixpoint is_equal helper fmla

let rec is_disj_normal_form fmla =
  match fmla with
    | `Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | `Sum(p, q) -> is_disj_normal_form p && is_disj_normal_form q
    | `Star(p)   -> is_conj_clause p
    | _          -> true

(* Assume in assoc form *)
let rec simplify_seq_actions fmla =
  assert(is_conj_clause fmla && is_actions fmla);
  let helper fmla =
    match fmla with
      | `Seq(`Store(_), `Red(n))                       -> `Red(n)
      | `Seq(`Red(n), `Store(_))                       -> `Red(n)
      | `Seq(`Red(_), `Red(n))                         -> `Red(n)
      | `Seq(`Store(_), `Store(n))                     -> `Store(n)
      | `Seq(`Mod(h1, v1), `Mod(h2, v2)) when h1 = h2  -> `Mod(h2, v2)
      | `Seq(`Store(n), `Mod(h, v)) when h = "mailbox" -> `Mod(h, v)
      | `Seq(`Mod(h, v), `Store(n)) when h = "mailbox" -> `Mod(h, n)
      | `Seq(`Red(n), `Mod(h, v))   when h = "envrcpt" -> `Red(v)
      | `Seq(`Mod(h, v), `Red(n))   when h = "envrcpt" -> `Red(n)
      | `Seq(`Mod(h, v), `Red(n))   when h = "mailbox" -> `Red(n)
      | _                                              -> fmla
  in match fmla with
    | `Seq(#action as p, `Seq((#action as q), r)) -> simplify_seq_actions (`Seq(helper (`Seq(p, q)), r))
    | `Seq(#action, #action)                      -> helper fmla
    | _                                           -> fmla

let is_convertible_to_snf fmla =
  not (contains_star fmla) && not (contains_mod fmla) && not (test_on fmla "mailbox")

let rec to_snf fmla =
  assert(is_convertible_to_snf fmla);
  let commute_tsts a tsts :> fmla =
    match a with
      | `Store(n) -> tsts
      | `Red(n)   -> if Test.test_on tsts "envrcpt" && Test.contra tsts (Test.mk_test "envrcpt" (Some n))
                     then (Test.mk_const false :> tests) else tsts
      | _         -> tsts
  in let helper f =
    match f with
      | `Seq(#action as p, ((#test | `Not #tests) as q))          -> `Seq(commute_tsts p q, p)
      | `Seq(#action, #action)                                    -> simplify_seq_actions f
      | `Seq(#action, (`Seq(#action, r) as t))                    -> to_snf (`Seq(simplify_seq_actions t, r))
      | `Seq(#action as p, `Seq(((#test | `Not #tests) as q), r)) -> `Seq(commute_tsts p q, to_snf (`Seq(p, r)))
      | `Seq(p, q)                                                -> `Seq(p, to_snf q)
      | `Sum(p, q)                                                -> `Sum(to_snf p, to_snf q)
      | _                                                         -> fmla
  in helper (assoc (to_disjunctive_normal_form fmla))

(* assume it is in SNF *)
let rec split_tests_actions ?tsts ?actions (fmla: fmla) =
  let merge fmla1 fmla2 =
    match fmla1, fmla2 with
      | None, None       -> None
      | Some f, None     -> Some f
      | None, Some f     -> Some f
      | Some f1, Some f2 -> Some (`Seq(f1, f2))
  in match fmla with
    | `Seq(p, q)                 -> let (tst1, act1) = (split_tests_actions ?tsts ?actions p)
                                    and (tst2, act2) = (split_tests_actions ?tsts ?actions q)
                                    in ((merge tst1 tst2), (merge act1 act2))
    | (#test | `Not #tests) as t -> (Some t, None)
    | #action                    -> (tsts, Some fmla)
    | _                          -> failwith "Invalid use of split_tests_actions"

exception Multiple_mtas;;

let rec which_mta fmla =
  let pick opt1 opt2 =
    match opt1, opt2 with
      | None, None     -> None
      | Some mta, None -> Some mta
      | None, Some mta -> Some mta
      | Some _, Some _ -> raise Multiple_mtas
  in match fmla with
    | `Seq(p1, p2)     -> pick (which_mta p1) (which_mta p2)
    | `Star(p)         -> which_mta p
    | `Test(_, v) as t -> if Test.test_on t "mta" then v else None
    | `Sum(_, _)       -> failwith "Mta resolution only applies to conjunctive clauses"
    | _                -> None

let rec remove_mta_tests fmla =
  match fmla with
    | `Seq(p1, p2)      -> `Seq(remove_mta_tests p1, remove_mta_tests p2)
    | `Sum(p1, p2)      -> `Sum(remove_mta_tests p1, remove_mta_tests p2)
    | `Star(p)          -> `Star(remove_mta_tests p)
    | #action as a      -> a
    | `Not(#tests as t) -> `Not(Test.remove_mta t)
    | #test as t        -> (Test.remove_mta t :> fmla)

let rec to_sieve fmla =
  let store_to_sieve s = Printf.sprintf "if environment :is \"location\" \"MDA\" {\n\t\tfileinto \"%s\";\n\t} else {\n\t\tkeep;\n\t}\n\t" s
  in let rec actions_to_sieve acts =
    match (simplify_seq_actions acts) with
      | `Mod(f, v)     -> failwith "Mod is not yet supported in Sieve compilation"
      | `Red(a)        -> assert (Util.is_address a); Printf.sprintf "redirect \"%s\";" a
      | `Store(m)      -> (store_to_sieve m)
      | `Seq(a1, a2)   -> Printf.sprintf "%s\n%s\n" (actions_to_sieve a1) (actions_to_sieve a2)
      | _              -> failwith "Unsupported action in Sieve compilation"
  in let rec tests_to_sieve (tsts: tests) =
    match (Test.simplify tsts) with
      | `Seq(t1, t2)               -> Printf.sprintf "%s, %s" (tests_to_sieve t1) (tests_to_sieve t2)
      | (#test | `Not #tests) as t -> Test.to_sieve t
      | _                          -> failwith "Unsupported test in Sieve compilation"
  in let helper (t, a) =
    match t, a with
      | Some tsts, Some actions -> Printf.sprintf "if allof(%s) {\n\t%s\n}" (tests_to_sieve tsts) (actions_to_sieve actions)
      | Some tsts, None         -> Printf.sprintf "if allof(%s) {\n\tkeep;\n}" (tests_to_sieve tsts)
      | None, Some actions      -> Printf.sprintf "%s" (actions_to_sieve actions)
      | None, None              -> ""
  in match (to_snf fmla) with
    | `Sum(p, q) -> Printf.sprintf "%s\n%s\n" (to_sieve p) (to_sieve q)
    | `Star(p)   -> failwith "Unsupported star operator in Sieve compilation"
    | f          -> helper (split_tests_actions ?tsts:None ?actions:None f)
