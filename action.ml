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
    | `Mod(f,v)                             -> Printf.sprintf "%s <- %s" f v
    | `Red(a)                               -> Printf.sprintf "Red[%s]" a
    | `Store(m)                             -> Printf.sprintf "Store[%s]" m
    | `Seq(a1, a2)                          -> Printf.sprintf "(%s ; %s)" (to_string a1) (to_string a2)
    | `Sum (a1, a2)                         -> Printf.sprintf "(%s + %s)" (to_string a1) (to_string a2)
    | `Star (a)                             -> Printf.sprintf "(%s)*" (to_string a)
    | (#test | `Not #tests) as t            -> Printf.sprintf "%s" (Test.to_string t)

let rec is_equal (fmla1: [< fmla ]) (fmla2: [< fmla ]) =
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

let is_test a =
  match a with
    | (`Const _ | `Not _ | `Test _) -> true
    | _                             -> false

let rec is_tests fmla =
  match fmla with
    | `Sum(p, q) -> is_tests p && is_tests q
    | `Seq(p, q) -> is_tests p && is_tests q
    | `Star(p)   -> is_tests p
    | t          -> is_test t

let is_simple_action a =
  match a with
    | #action    -> true
    | _          -> false

let rec is_actions fmla =
  match fmla with
    | `Sum(p, q) -> is_actions p && is_actions q
    | `Seq(p, q) -> is_actions p && is_actions q
    | `Star(p)   -> is_actions p
    | #action    -> true
    | _          -> false

let rec test_on tsts a =
  match tsts, a with
    | `Sum(p, q), _                       -> test_on p a || test_on q a
    | `Seq(p, q), _                       -> test_on p a || test_on q a
    | `Star(p), _                         -> test_on p a
    | (#test | `Not #tests) as t, `Mod(f, v)              -> Test.test_on t f v
    | (#test | `Not #tests) as t, `Red(a)                 -> Test.test_on t "envrcpt" a
    | (#test | `Not #tests) as t, `Store(m)               -> Test.test_on t "mailbox" m
    | _ , _                               -> false

let rec eval_test_on (fmla: fmla) (a: fmla):bool =
  assert(is_tests fmla);
  match fmla, a with
    | `Sum(p, q), _                           -> eval_test_on p a || eval_test_on q a
    | `Seq(p, q), _                           -> eval_test_on p a && eval_test_on q a
    | `Star(p), _                             -> eval_test_on p a
    | (#test | `Not #tests) as t, `Mod(f, v)  -> Test.eval_on t f v
    | (#test | `Not #tests) as t, `Red(a)     -> Test.eval_on t "envrcpt" a
    | (#test | `Not #tests) as t, `Store(m)   -> Test.eval_on t "mailbox" m
    | _                                       -> true

let rec is_conj_clause fmla =
  match fmla with
    | `Sum(p, q) -> false
    | `Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | `Star p    -> is_conj_clause p
    | _         -> true

let to_disj_normal_form fmla =
  let rec helper fmla =
    match fmla with
      | `Seq(`Sum (p, q), r)        -> `Sum(helper (`Seq(p, r)), helper (`Seq(q, r)))
      | `Seq(p, `Sum(q, r))         -> `Sum(helper (`Seq(p, q)), helper (`Seq(p, r)))
      | `Seq(p, q)                  -> `Seq(helper p, helper q)
      | `Sum(p, q)                  -> `Sum(helper p, helper q)
      | `Star(p)                    -> `Star(helper p)
      | (#test | `Not #tests) as t  -> ((Test.to_disj_normal_form t) :> fmla)
      | #action as a                -> a
  in Util.fixpoint is_equal helper fmla

let rec is_disj_normal_form fmla =
  match fmla with
    | `Seq(p, q) -> is_conj_clause p && is_conj_clause q
    | `Sum(p, q) -> is_disj_normal_form p && is_disj_normal_form q
    | `Star(p)   -> is_conj_clause p
    | _         -> true

(* Assume no tests on mailbox *)
let rec commute_actions fmla =
  let eval_single tsts p :> fmla =
    if eval_test_on tsts p then Test.mk_const true else Test.mk_const false
  in let commute_single f =
    match f with
      | `Seq(p, q) when is_tests q -> if not (test_on q p) then `Seq(q, p) else `Seq(eval_single q p, q)
      | _                          -> f
  in let commute_deep f =
    match f with
      | `Seq(`Seq(r, p), q)  -> if (is_tests q) then (if not (test_on q p) then `Seq(`Seq(r, q), p) else `Seq(`Seq(r, eval_single q p), q)) else f
      | _          -> f
  in match fmla with
    | `Seq(`Mod(f1, v1), q)         -> commute_single fmla
    | `Seq(`Red(a), q)              -> commute_single fmla
    | `Seq(`Store(m), q)            -> commute_single fmla
    | `Seq(`Seq(p, `Mod(f1, v1)), q) -> commute_deep fmla
    | `Seq(`Seq(p, `Red(a)), q)      -> commute_deep fmla
    | `Seq(`Seq(p, `Store(m)), q)    -> commute_deep fmla
    | `Seq(p, q)                   -> `Seq(commute_actions p, commute_actions q)
    | _                           -> fmla

let rec simplify_seq_actions fmla =
  assert(is_conj_clause fmla && is_actions fmla);
  let simplify_simple fmla =
    match fmla with
      | `Seq(`Store(_), `Red(n))                       -> `Red(n)
      | `Seq(`Red(n), `Store(_))                       -> `Red(n)
      | `Seq(`Store(m), `Store(n))                     -> `Store(n)
      | `Seq(`Mod(h1, v1), `Mod(h2, v2)) when h1 = h2  -> `Mod(h2, v2)
      | `Seq(`Store(n), `Mod(h, v)) when h = "mailbox" -> `Mod(h, v)
      | `Seq(`Mod(h, v), `Store(n)) when h = "mailbox" -> `Mod(h, n)
      | `Seq(`Red(n), `Mod(h, v))   when h = "envrcpt" -> `Red(v)
      | `Seq(`Mod(h, v), `Red(n))   when h = "envrcpt" -> `Red(n)
      | `Seq(`Mod(h, v), `Red(n))   when h = "mailbox" -> `Red(n)
      | _                                              -> fmla
  in match fmla with
    | `Seq(`Seq(p, q), r) when is_simple_action (simplify_simple (`Seq(q, r))) -> simplify_seq_actions (`Seq(p, (simplify_simple (`Seq(q, r)))))
    | `Seq(`Seq(p, q), r)                                                    -> `Seq((simplify_seq_actions (`Seq(p, q))), r)
    | `Seq(p, q) when is_simple_action p && is_simple_action q -> simplify_simple fmla
    | _                -> fmla

let rec simplify fmla =
  assert (is_disj_normal_form fmla);
  let simplify_drop fmla =
    assert(is_conj_clause fmla);
    let rec contains_drop f =
      match f with
        | `Seq(p, q) -> contains_drop p || contains_drop q
        | `Star(p)   -> contains_drop p
        | (#test | `Not #tests) as t   -> Test.is_drop t
        | _         -> false
    in if contains_drop fmla then ((Test.mk_const false) :> fmla) else fmla
  in let rec assoc fmla =
    match fmla with
      | `Sum(p, q)           -> `Sum(assoc p, assoc q)
      | `Seq(p, (`Seq(q,r))) -> `Seq(`Seq(p, q), r)
      | `Seq(p, q)           -> `Seq(assoc p, assoc q)
      | `Star(p)             -> `Star(assoc p)
      | _                    -> fmla
  in match fmla with
    | `Seq(p, q) -> simplify_seq_actions (simplify_drop (Util.fixpoint is_equal (Fn.compose commute_actions assoc) fmla))
    | `Sum(p, q) -> `Sum(simplify p, simplify q)
    | `Star(p)   -> simplify p
    | _          -> fmla

(* assume it is in assoc form *)
let rec split_tests_actions fmla =
  match fmla with
    | `Sum(p, q)                                                 -> `Sum(split_tests_actions p, split_tests_actions q)
    | `Seq(`Seq(p, q), r) when is_actions r && is_simple_action q -> split_tests_actions (`Seq(p, (`Seq(q, r))))
    | `Star(p)                                                   -> `Star(split_tests_actions p)
    | _                                                         -> fmla

(* TODO: add require only if needed. Assert its in correct form *)
let to_sieve fmla =
  let require_extensions () =
    Printf.sprintf "reqire[\"fileinto\", \"envelope\"];\n"
  in let rec actions_to_sieve acts =
    match acts with
      | `Mod(f, v)     -> failwith "`Mod is not supported in Sieve translation"
      | `Red(a)        -> assert (Util.is_address a); Printf.sprintf "`Redirect \"%s\";" a
      | `Store(m)      -> Printf.sprintf "fileinto \"%s\";" m
      | `Seq(a1, a2)   -> Printf.sprintf "%s\n%s\n" (actions_to_sieve a1) (actions_to_sieve a2)
      | _              -> failwith "Unsupported action in Sieve translation"
  in let rec tests_to_sieve tsts =
    assert (is_tests tsts);
    match tsts with
      | `Seq(t1, t2)      -> Printf.sprintf "%s, %s" (tests_to_sieve t1) (tests_to_sieve t2)
      | (#test | `Not #tests) as t   -> Test.to_sieve t
      | _                 -> failwith "Unsupported test in Sieve translation"
  (* TODO: add assert its in right form, no `Star *)
  in let rec helper p =
    match p with
      | `Seq(tsts, actions) when is_actions actions -> Printf.sprintf "if allof(%s) {\n %s \n}" (tests_to_sieve tsts) (actions_to_sieve actions)
      | (`Seq(_, _) | #test | `Not #tests)   -> Printf.sprintf "if allof(%s) {\nkeep;\n}" (tests_to_sieve p)
      | `Star(q)            -> helper q
      | `Sum(p1, p2)        -> Printf.sprintf "%s\n %s\n" (helper p1) (helper p2)
      | #action as a         -> actions_to_sieve a
  in Printf.sprintf "%s%s\ndiscard;\n" (require_extensions ()) (helper (split_tests_actions (simplify (to_disj_normal_form fmla))))
