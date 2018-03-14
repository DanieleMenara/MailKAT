open Core.Std

type test =
  | Const of bool
  | Neg of test
  | Test of (string * string)
  | Sum of (test * test)
  | Seq of (test * test)

let rec is_drop tst =
  match tst with
    | Seq(t1, t2) -> is_drop t1 || is_drop t2
    | Neg(t)      -> not (is_drop t)
    | Const(c)    -> not c
    | _           -> false

let mk_sum t1 t2 = Sum(t1, t2)

let mk_seq t1 t2 = Seq(t1, t2)

let is_sum tst =
  match tst with
    | Sum(_, _) -> true
    | _         -> false

let rec to_string t =
  match t with
    | Const(c)     -> if c then Printf.sprintf "1" else Printf.sprintf "0"
    | Neg t        -> Printf.sprintf "%s" (to_string t)
    | Test (f, v)  -> Printf.sprintf "%s = %s" f v
    | Seq(t1, t2)  -> Printf.sprintf "%s ; %s" (to_string t1) (to_string t2)
    | Sum(t1, t2)  -> Printf.sprintf "%s + %s" (to_string t1) (to_string t2)

let rec is_equal t1 t2 =
  match t1, t2 with
    | Const(c1), Const(c2)              -> c1 = c2
    | Neg(s1), Neg(s2)                  -> is_equal s1 s2
    | Test(f1, v1), Test(f2, v2)        -> f1 = f2 && v1 = v2
    | Seq(t1, t2), Seq(t3, t4)          -> (is_equal t1 t3) && (is_equal t2 t4)
    | Sum(t1, t2), Sum(t3, t4)          -> (is_equal t1 t3) && (is_equal t2 t4)
    | _, _                              -> false

let rec test_on tst f v =
  match tst with
    | Test(f1, v1)  -> f = f1
    | Neg(t)        -> test_on t f v
    | Seq(t1, t2)   -> (test_on t1 f v) || (test_on t2 f v)
    | Sum(t1, t2)   -> (test_on t1 f v) || (test_on t2 f v)
    | Const(c)      -> false

let rec eval_on tst f v =
  match tst with
    | Test(f1, v1)  -> (f = f1 && (v = "*" || v = v1)) || not (f = f1)
    | Neg(t)        -> not (test_on t f v)
    | Seq(t1, t2)   -> (eval_on t1 f v) && (eval_on t2 f v)
    | Sum(t1, t2)   -> (eval_on t1 f v) || (eval_on t2 f v)
    | Const(c)      -> c

let is_envelope s =
  match s with
    | "envrcpt" | "envfrom" -> true
    | _                     -> false

let rec to_sieve tst =
  let header_to_sieve s =
    match s with
      | "envrcpt" -> "to"
      | "envform" -> "from"
      | _         -> s
  in match tst with
    | Const(c)      -> if c then Printf.sprintf "true" else Printf.sprintf "false"
    | Neg(t)        -> Printf.sprintf "not %s" (to_sieve t)
    | Test(f, v)    -> Printf.sprintf "%s :is \"%s\" \"%s\"" (if Util.is_address v then "address" else
                       if is_envelope f then "envelope" else "header") (header_to_sieve f) v
    | Seq(t1, t2)   -> Printf.sprintf "allof (%s, %s)" (to_sieve t1) (to_sieve t2)
    | Sum(t1, t2)   -> Printf.sprintf "anyof (%s, %s)" (to_sieve t1) (to_sieve t2)
