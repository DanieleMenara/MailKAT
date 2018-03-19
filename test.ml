open Core

type ('a, 'b) seq = [ `Seq of 'a * 'b ]
type ('a, 'b) sum = [ `Sum of 'a * 'b ]

type test =
  [ `Const of bool
  | `Test of (string * string) ]

type 'a neg = [ `Not of 'a ] constraint 'a = [< test | ('a,'a) seq | ('a,'a) sum | `Not of 'a ]

type tests =
  [ test
  | (tests, tests) seq
  | (tests, tests) sum
  | tests neg ]


let rec is_conj_clause tst  =
  match tst with
    | `Const(c)          -> true
    | `Not(`Seq(_, _))   -> false (* negations have to be pushed all the way down *)
    | `Not(t)            -> is_conj_clause t
    | `Test(_, _)        -> true
    | `Sum(_, _)         -> false
    | `Seq(t1, t2)       -> (is_conj_clause t1) && (is_conj_clause t2)
    | _                  -> false

let rec is_drop tst =
  assert(is_conj_clause tst);
  match tst with
    | `Seq(t1, t2)    -> is_drop t1 || is_drop t2
    | `Not(`Const(c))  -> c
    | `Const(c)       -> not c
    | _              -> false

let is_keep t =
  assert(is_conj_clause t);
  let rec helper tst =
    match tst with
      | `Seq(t1, t2)    -> helper t1 || helper t2
      | `Not(`Const(c))  -> not c
      | `Const(c)       -> c
      | _              -> false
  in (helper t) && not (is_drop t)

let mk_sum t1 t2 = `Sum(t1, t2)

let mk_seq t1 t2 = `Seq(t1, t2)

let mk_const c = `Const(c)

let is_sum tst =
  match tst with
    | `Sum(_, _) -> true
    | _         -> false

let rec to_string (t: tests) =
  match t with
    | `Not(t)       -> Printf.sprintf "!(%s)" (to_string t)
    | `Const(c)     -> if c then Printf.sprintf "1" else Printf.sprintf "0"
    | `Test (f, v)  -> Printf.sprintf "%s = %s" f v
    | `Seq(t1, t2)  -> Printf.sprintf "%s ; %s" (to_string t1) (to_string t2)
    | `Sum(t1, t2)  -> Printf.sprintf "%s + %s" (to_string t1) (to_string t2)
    | _             -> failwith "Not supported test"

let rec is_equal t1 t2 =
  match t1, t2 with
    | `Const(c1), `Const(c2)              -> c1 = c2
    | `Not(s1), `Not(s2)                  -> is_equal s1 s2
    | `Test(f1, v1), `Test(f2, v2)        -> f1 = f2 && v1 = v2
    | `Seq(t1, t2), `Seq(t3, t4)          -> (is_equal t1 t3) && (is_equal t2 t4)
    | `Sum(t1, t2), `Sum(t3, t4)          -> (is_equal t1 t3) && (is_equal t2 t4)
    | _, _                                -> false

let rec test_on tst f v =
  match tst with
    | `Test(f1, v1)  -> f = f1
    | `Not(t)        -> test_on t f v
    | `Seq(t1, t2)   -> (test_on t1 f v) || (test_on t2 f v)
    | `Sum(t1, t2)   -> (test_on t1 f v) || (test_on t2 f v)
    | `Const(c)      -> false

let rec eval_on tst f v =
  match tst with
    | `Test(f1, v1)  -> (f = f1 && (v = "*" || v = v1)) || not (f = f1)
    | `Not(t)        -> not (test_on t f v)
    | `Seq(t1, t2)   -> (eval_on t1 f v) && (eval_on t2 f v)
    | `Sum(t1, t2)   -> (eval_on t1 f v) || (eval_on t2 f v)
    | `Const(c)      -> c

let is_envelope s =
  match s with
    | "envrcpt" | "envfrom" -> true
    | _                     -> false

let to_disj_normal_form (tst: tests): [> tests] =
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
    | `Test(f, v)    -> Printf.sprintf "%s :is \"%s\" \"%s\"" (if is_envelope f then "envelope" else
                       if Util.is_address v then "address" else "header") (header_to_sieve f) v
    | `Seq(t1, t2)   -> Printf.sprintf "allof (%s, %s)" (to_sieve t1) (to_sieve t2)
    | `Sum(t1, t2)   -> Printf.sprintf "anyof (%s, %s)" (to_sieve t1) (to_sieve t2)
