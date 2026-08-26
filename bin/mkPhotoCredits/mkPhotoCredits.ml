(* Copyright (c) 2026 H.Gouraud *)
open Gwtolatex

(* Post-pass for the global <x PhotoCredits> command (no parameter).

   mkTex has already emitted the marker line Sutil.photocredits_marker (an inert
   LaTeX comment) where the credits table should appear, and written
   tmp/<family>.pcredits, one record per printed image:
       kind<TAB>aux_label<TAB>id<TAB>number<TAB>file<TAB>credit
   (kind = photo | portrait; number = ch.sec.nb).

   This pass groups the credited images BY CREDIT SOURCE, lists their numbers,
   and replaces the marker in tmp/<family>.tex - no page numbers, no .aux
   needed. Under <x ShowNoCredits on> a second block lists the credit-less
   images with their id and file so they are easy to complete. A second
   pdflatex then typesets the result. *)

let family = ref ""
let verbose = ref false
let debug = ref 0

(* The global <x PhotoCredits> table groups the credited photos BY CREDIT
   SOURCE and lists their ch.sec.nb numbers - no page, no file name, no person.
   So page numbers (and the .aux) are no longer needed here. *)

(* returns (show_no_credits, records). The header line "#shownocredits N"
   carries the <x ShowNoCredits> flag; every other line is a tab-separated
   record. *)
let read_pcredits file =
  let recs = ref [] in
  let show_nc = ref false in
  if Sys.file_exists file then (
    let ic = open_in file in
    (try
       while true do
         let line = input_line ic in
         if Sutil.start_with "#shownocredits" 0 line then
           show_nc :=
             String.trim (String.sub line 14 (String.length line - 14)) = "1"
         else
           match String.split_on_char '\t' line with
           | [ kind; label; id; number; file; credit ] ->
               recs := (kind, label, id, number, file, credit) :: !recs
           | [ kind; label; id; number; file ] ->
               recs := (kind, label, id, number, file, "") :: !recs
           | _ -> ()
       done
     with End_of_file -> ());
    close_in ic);
  (!show_nc, List.rev !recs)

(* Natural order for photo numbers like "4.11.1" (so 4.8.2 < 4.11.1). *)
let compare_number a b =
  let split s =
    List.map
      (fun p -> try int_of_string p with _ -> 0)
      (String.split_on_char '.' s)
  in
  compare (split a) (split b)

(* Replace every occurrence of [sub] by [by] (all occurrences, whole file). *)
let replace_all_sub sub by s =
  let ls = String.length s and lsub = String.length sub in
  if lsub = 0 then s
  else begin
    let buf = Buffer.create ls in
    let i = ref 0 in
    while !i <= ls - lsub do
      if String.sub s !i lsub = sub then (
        Buffer.add_string buf by;
        i := !i + lsub)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.add_string buf (String.sub s !i (ls - !i));
    Buffer.contents buf
  end

(* Escape the few LaTeX specials a file name may contain (mainly '_'). *)
let tex_escape s =
  let b = Buffer.create (String.length s + 4) in
  String.iter
    (fun c ->
      match c with
      | '_' | '#' | '%' | '&' | '$' | '{' | '}' ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | '~' | '^' | '\\' -> Buffer.add_string b (Printf.sprintf "\\char`\\%c" c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let word kind = if kind = "portrait" then "portrait" else "photo"

(* Credited list, grouped by credit source (alphabetical), each followed by its
   ch.sec.nb numbers (natural order, de-duplicated). No file name, no person,
   no page. Records with no number (a portrait of a person with no individual
   page) cannot be referenced this way and are skipped - an accepted limit. *)
let build_by_credit records =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (_kind, _label, _id, number, _file, credit) ->
      if number <> "" then
        let prev = try Hashtbl.find tbl credit with Not_found -> [] in
        Hashtbl.replace tbl credit (number :: prev))
    records;
  let credits =
    Hashtbl.fold (fun c _ acc -> c :: acc) tbl [] |> List.sort compare
  in
  let buf = Buffer.create 4096 in
  List.iter
    (fun credit ->
      let nums = Hashtbl.find tbl credit |> List.sort_uniq compare_number in
      Buffer.add_string buf
        (Format.sprintf "\\par\\noindent \\textbf{%s}~: %s.\n" credit
           (String.concat ", " nums)))
    credits;
  Buffer.contents buf

(* Flat list of the credit-less images, sorted by number, each shown with its
   who_is_where id and file so the entry is easy to find. *)
let build_no_credit records =
  records
  |> List.sort (fun (_, _, _, na, _, _) (_, _, _, nb, _, _) ->
      compare_number na nb)
  |> List.map (fun (kind, _label, id, number, file, _credit) ->
      let w = if number = "" then word kind else word kind ^ " " ^ number in
      Format.sprintf "%s [id %s]%s" w id
        (if file <> "" then Format.sprintf " (\\texttt{%s})" (tex_escape file)
         else ""))
  |> String.concat ", "

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-v", Arg.Set verbose, " verbose or quiet.");
    ]
  in
  let speclist = Arg.align (List.sort compare speclist) in
  Arg.parse speclist (fun _ -> ()) usage;

  let tex_file = String.concat Filename.dir_sep [ "tmp"; !family ^ ".tex" ] in
  let pcr_file =
    String.concat Filename.dir_sep [ "tmp"; !family ^ ".pcredits" ]
  in

  Printf.eprintf "This is \027[32mmkPhotoCredits\027[0m version %s for %s\n"
    Sutil.version !family;
  flush stderr;

  if not (Sys.file_exists tex_file) then (
    Printf.eprintf "PhotoCredits: %s not found, nothing to do\n" tex_file;
    exit 0);

  let ic = open_in_bin tex_file in
  let tex = really_input_string ic (in_channel_length ic) in
  close_in ic;

  if Sutil.contains_index tex Sutil.photocredits_marker = -1 then (
    (* the document never used <x PhotoCredits> - leave it untouched *)
    if !verbose then Printf.eprintf "PhotoCredits: no marker, nothing to do\n";
    exit 0);

  let show_nc, records = read_pcredits pcr_file in
  (* credited photos feed the by-source list; the credit-less ones appear in
     their own list only under <x ShowNoCredits on>. *)
  let credited = List.filter (fun (_, _, _, _, _, c) -> c <> "") records in
  let no_credit = List.filter (fun (_, _, _, _, _, c) -> c = "") records in

  let credited_list =
    let l = build_by_credit credited in
    if l = "" then "\\par\\noindent (aucun crédit photo).\n" else l
  in
  let no_credit_block =
    if (not show_nc) || no_credit = [] then ""
    else
      Format.sprintf
        "\\par\\medskip\\noindent\\textbf{Images sans crédit}~: %s.\n"
        (build_no_credit no_credit)
  in
  let content = credited_list ^ no_credit_block in
  let tex = replace_all_sub Sutil.photocredits_marker content tex in
  let oc = open_out_bin tex_file in
  output_string oc tex;
  close_out oc;

  Printf.eprintf
    "PhotoCredits: %d credited photo(s) grouped by source; %d without credit%s\n"
    (List.length credited) (List.length no_credit)
    (if show_nc then " (listed)" else " (hidden)");
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
