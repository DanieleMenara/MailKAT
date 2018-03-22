open Action
open Printf
open Util

let split_by_mta fmla =
  let tbl () = Hashtbl.create 10
  in let merge_entries table key fmla =
    match Hashtbl.find_opt table key with
      | Some v -> Hashtbl.replace table key (Action.mk_sum v fmla)
      | None   -> Hashtbl.add table key fmla
  in let add_to_tbl tbl conj_clause =
    try
      let mta = which_mta conj_clause
      in if is_none mta then merge_entries tbl "All" conj_clause
      else merge_entries tbl (option_get mta) (remove_mta_tests conj_clause)
    with
      Multiple_mtas -> ()
  in let rec map_to_mta tbl f =
    match f with
      | `Sum(p, q)  -> map_to_mta tbl p; map_to_mta tbl q
      | `Star _     -> failwith "Program needs to be star free to be split by mta"
      | `Seq(_, _)  -> add_to_tbl tbl f; tbl
      | _           -> add_to_tbl tbl f; tbl
  in let table_to_list table =
    match Hashtbl.find_opt table "All" with
      | Some all -> Hashtbl.remove table "All";
                    Hashtbl.fold (fun k v acc -> (k, (Action.mk_sum v all))::acc) table []
      | None     -> Hashtbl.fold (fun k v acc -> (k, v)::acc) table []
  in table_to_list (map_to_mta (tbl ();) (to_disj_normal_form fmla))

let to_sieve ?(print=false) prog =
  let require_extensions () =
    Printf.sprintf "require[\"fileinto\", \"envelope\"];\n"
  in let rec helper prog =
    match prog with
      | []                   -> ()
      | (filename, p1)::tail -> let out_ch = open_out (String.concat "." [filename; "sieve"])
                                in let sieve = Printf.sprintf "%s\n\n%s\ndiscard;\n" (require_extensions ())
                                               (Action.to_sieve p1)
                                in (if print then printf "------------\n%s------------\n" sieve);
                                fprintf out_ch "%s" sieve;
                                close_out out_ch;
                                helper tail
  in if contains_star prog then failwith "Cannot convert to Sieve program with Kleene Star operator"
     else helper (split_by_mta prog)
