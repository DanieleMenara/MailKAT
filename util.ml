open Str

let rec fixpoint eq f x =
  let y = f x in if eq x y then x else fixpoint eq f y

let is_address s =
  string_match (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]+") s 0
