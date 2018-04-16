open Core
open Lexer
open Lexing
open Program

(* Functions print_position, parse_with_error, parse_and_print and loop from:
   Chapter 16, Real World OCaml: Functional programming for the masses.
   "O'Reilly Media, Inc.".
   Minsky, Y., Madhavapeddy, A., & Hickey, J. (2013).*)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print ?(print=false) lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    Program.to_sieve ~print value;
    parse_and_print lexbuf
  | None -> ()

let loop ?(print=false) filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print ~print lexbuf;
  In_channel.close inx;;

module Make() =
  struct
    let compile ?(print=false) filename =
      let start = Unix.gettimeofday () in
      let code = loop ~print filename in
      let stop = Unix.gettimeofday () in
      code;
      Printf.printf "Compilation time: %fs\n%!" (stop -. start);;
end
