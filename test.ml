open Core.Std

type test =
  | Const of bool
  | Neg of test
  | Test of (string * string)

let rec to_string t =
  match t with
    | Const(c)     -> if c then Printf.sprintf "1" else Printf.sprintf "0"
    | Neg t        -> Printf.sprintf "%s" (to_string t)
    | Test (f, v)  -> Printf.sprintf "%s = %s" f v
