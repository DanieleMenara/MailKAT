module Sieve = Compiler.Make()

let show_output = ref false
let output_folder = ref ""
let to_sieve = ref ""

let optlist = ref [
  ("-p", Arg.Set show_output,": output to console.");
  ("-to-sieve", Arg.Set_string to_sieve,": compile <file> to sieve.");
  ("-o", Arg.Set_string output_folder,"It saves the output to the specified <output_folder>");
]

let usage =
  ref ("usage: " ^ Sys.argv.(0) ^ " [-p] -to-sieve <file> [-o <output_folder>]")

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string !optlist !usage) ;
  exit 1

let () =
    Arg.parse !optlist (fun _ -> raise (Arg.Bad "Stray argument found.")) !usage ;
    if not (!to_sieve="") then
      Sieve.compile ~print:!show_output ~folder:!output_folder !to_sieve
    else
      die "Invalid arguments or flags."
