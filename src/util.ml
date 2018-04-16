open Str

(** Perform fixpoint computation. i.e. keep apply f to x until there is no change
    (equality is determined by [eq] argument)*)
let rec fixpoint eq f x =
  let y = f x in if eq x y then x else fixpoint eq f y

(** Checks that the string [s] is an address with a regex. *)
let is_address s =
  string_match (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]+") s 0

(** Get value friom ['a option] *)
let option_get ~if_none a =
  match a with
    | Some v -> v
    | None   -> if_none

(** Check if ['a option] is none *)
let is_none a =
  match a with
    | Some _ -> false
    | None   -> true

(** Sanitize header string *)
let sanitize_header h =
  String.trim (String.lowercase_ascii h)
