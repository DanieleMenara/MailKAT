open Core

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

let mk_const c = `Const(c)

let rec is_conj_clause tst  =
  match tst with
    | #test              -> true
    | `Not(`Seq(_, _))   -> false (* negations have to be pushed all the way down *)
    | `Not(t)            -> is_conj_clause t
    | `Sum(_, _)         -> false
    | `Seq(t1, t2)       -> (is_conj_clause t1) && (is_conj_clause t2)
    | _                  -> false

let rec is_drop tst =
  assert(is_conj_clause tst);
  match tst with
    | `Seq(t1, t2)    -> is_drop t1 || is_drop t2
    | `Not(`Const(c)) -> c
    | `Const(c)       -> not c
    | _               -> false

let rec to_string (t: tests) =
  match t with
    | `Not(t)       -> Printf.sprintf "!(%s)" (to_string t)
    | `Const(c)     -> if c then Printf.sprintf "1" else Printf.sprintf "0"
    | `Test(h, v)   -> Printf.sprintf "%s = %s" h v
    | `Seq(t1, t2)  -> Printf.sprintf "%s ; %s" (to_string t1) (to_string t2)
    | `Sum(t1, t2)  -> Printf.sprintf "%s + %s" (to_string t1) (to_string t2)

let rec is_equal t1 t2 =
  match t1, t2 with
    | `Const(c1), `Const(c2)              -> c1 = c2
    | `Not(s1), `Not(s2)                  -> is_equal s1 s2
    | `Test(h1, v1), `Test(h2, v2)        -> h1 = h2 && v1 = v2
    | `Seq(t1, t2), `Seq(t3, t4)          -> (is_equal t1 t3) && (is_equal t2 t4)
    | `Sum(t1, t2), `Sum(t3, t4)          -> (is_equal t1 t3) && (is_equal t2 t4)
    | _, _                                -> false

let rec test_on tst h v =
  match tst with
    | `Test(h1, v1)  -> h = h1
    | `Not(t)        -> test_on t h v
    | `Seq(t1, t2)   -> (test_on t1 h v) || (test_on t2 h v)
    | `Sum(t1, t2)   -> (test_on t1 h v) || (test_on t2 h v)
    | `Const(c)      -> false

let rec eval_on tst h v =
  match tst with
    | `Test(h1, v1)  -> (h = h1 && (v = "*" || v = v1)) || not (h = h1)
    | `Not(t)        -> not (test_on t h v)
    | `Seq(t1, t2)   -> (eval_on t1 h v) && (eval_on t2 h v)
    | `Sum(t1, t2)   -> (eval_on t1 h v) || (eval_on t2 h v)
    | `Const(c)      -> c

let is_envelope s =
  match s with
    | "envrcpt" | "envfrom" -> true
    | _                     -> false

let to_disj_normal_form tst =
  let rec helper t =
    match t with
      | `Seq(`Sum (p, q), r) -> `Sum(helper (`Seq(p, r)), helper (`Seq(q, r)))
      | `Seq(p, `Sum(q, r))  -> `Sum(helper (`Seq(p, q)), helper (`Seq(p, r)))
      | `Seq(p, q)           -> `Seq(helper p, helper q)
      | `Sum(p, q)           -> `Sum(helper p, helper q)
      | `Not(`Sum(t1, t2))   -> `Seq(`Not(t1), `Not(t2))
      | `Not(`Seq(t1, t2))   -> `Sum(`Not(t1), `Not(t2))
      | _                    -> t
  in Util.fixpoint is_equal helper tst

let rec to_sieve tst =
  let header_to_sieve s =
    match s with
      | "envrcpt" -> "to"
      | "envform" -> "from"
      | _         -> s
  in match tst with
    | `Const(c)      -> if c then Printf.sprintf "true" else Printf.sprintf "false"
    | `Not(t)        -> Printf.sprintf "not %s" (to_sieve t)
    | `Test(h, v)    -> Printf.sprintf "%s :is \"%s\" \"%s\"" (if is_envelope h then "envelope" else
                       if Util.is_address v then "address" else "header") (header_to_sieve h) v
    | `Seq(t1, t2)   -> Printf.sprintf "allof (%s, %s)" (to_sieve t1) (to_sieve t2)
    | `Sum(t1, t2)   -> Printf.sprintf "anyof (%s, %s)" (to_sieve t1) (to_sieve t2)
