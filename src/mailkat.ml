
module Sieve = Compiler.Make()

let show_output = ref false
let to_sieve = ref ""

let optlist = ref [
  ("-p", Arg.Set show_output,": output to console.");
  ("-to-sieve", Arg.Set_string to_sieve,": compile <file> to sieve.");
]

let usage =
  ref ("usage: " ^ Sys.argv.(0) ^ " [-p] -to-sieve <file>")

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string !optlist !usage) ;
  exit 1

let () =
    Arg.parse !optlist (fun _ -> raise (Arg.Bad "Stray argument found.")) !usage ;
    if not (!to_sieve="") then
      Sieve.compile ~print:!show_output !to_sieve
    else
      die "Invalid arguments or flags."
