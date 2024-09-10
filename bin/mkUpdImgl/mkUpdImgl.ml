(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

(*
scan family.tex file to update the list
of images where a person is present
*)

(* execution context *)
let base = ref ""
let passwd = ref ""
let gw_dir = ref (try Sys.getenv "GW_BIN" with Not_found -> "./")
let family = ref ""
let out_file = ref ""
let debug = ref 0
let pass = ref 0
let dev = ref false
let verbose = ref false
let no_fail = ref false
let dict4 = Hashtbl.create 100
let line_cnt = ref 0
let section_mark = ref ""

(* Assumes we are running in bases folder GeneWeb security constraint *)
let livres = ref ""
let bases = ref ""
let test = ref false
let test_nb = ref 0

(** Removes spaces at the begining and at the end of string. *)
let cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len - 1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)

(** from Geneweb.Mutil *)
let strip_tr_sp s =
  let b = Buffer.create (String.length s) in
  let len =
    let rec loop i =
      if i < 0 then 0
      else
        match s.[i] with ' ' | '\t' | '\r' | '\n' -> loop (i - 1) | _ -> i + 1
    in
    loop (String.length s - 1)
  in
  let rec loop i =
    if i = len then Buffer.contents b
    else
      match s.[i] with
      | '\r' -> loop (i + 1)
      | ' ' | '\t' ->
          let rec loop0 j =
            if j = len then Buffer.contents b
            else
              match s.[j] with
              | ' ' | '\t' | '\r' -> loop0 (j + 1)
              | '\n' -> loop j
              | _ ->
                  Buffer.add_char b s.[i];
                  loop (i + 1)
          in
          loop0 (i + 1)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0

(** Read line from input channel. *)
let input_a_line ic =
  let line = input_line ic in
  incr line_cnt;
  if String.length line > 0 && line.[String.length line - 1] = '\r' then
    String.sub line 0 (String.length line - 1)
  else line

(** Read a line. If line is empty or only contains a comment (#), then read next line  *)
let input_real_line ic = input_line ic

let output_string_nl oc str =
  output_string oc str;
  output_string oc "\n"

let process ich och dict1 dict2 dict4 img_ok_list =
  try
    while true do
      let line = input_real_line ich |> Sutil.strip_nl in
      if Sutil.start_with "sectionmark" 0 line then
        section_mark := String.sub line 12 (String.length line - 12)
      else if Sutil.start_with "(IMGID" 0 line then (
        let parts = String.split_on_char ' ' line in
        if List.length parts <> 3 then (
          Printf.eprintf "Bad IMGID syntax: %s\n" line;
          output_string och (strip_tr_sp line ^ "\n"))
        else
          let image_id = int_of_string (List.nth parts 1) in
          let _key_str = List.nth parts 2 |> Sutil.strip_c ')' in
          let anx_page, desc, _fname, _key_l, _key_l_2, image_occ =
            Hashtbl.find dict1 image_id
          in
          let rec loop image_occ =
            match image_occ with
            | [] ->
                if not (List.mem image_id img_ok_list) then
                  output_string och
                    (if anx_page <> 0 then
                     Format.sprintf "annexe page %d" anx_page
                    else "(???)") (* empty string is not acceptable for LaTeX *)
            | occ :: image_occ ->
                let parts = String.split_on_char '.' occ in
                let nbr =
                  int_of_string (List.nth parts (List.length parts - 1))
                in
                let p1 = String.split_on_char '.' occ in
                let l1 = List.length p1 in
                let p1 =
                  let rec loop n acc p1 =
                    match p1 with
                    | p :: p1 when n > 0 -> loop (n - 1) (p :: acc) p1
                    | p :: p1 -> List.rev acc
                    | _ -> assert false
                  in
                  loop (l1 - 1) [] p1
                in
                let occ2 = String.concat "." p1 in
                output_string och
                  (if Sutil.start_with occ2 0 !section_mark then
                   Format.sprintf "image %s.%d ci-dessous" occ2 nbr
                  else
                    Format.sprintf
                      "\\ref{img_ref_%d.%s}.%d page \\pageref{img_ref_%d.%s}"
                      image_id occ nbr image_id occ);
                if image_occ <> [] then output_string och ", ";
                loop image_occ
          in
          loop image_occ;
          output_string och ";\n")
      else if line <> "" then output_string och (strip_tr_sp line ^ "\n")
    done
  with End_of_file -> ()

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> base := x), " Which base.");
      ("-gw", Arg.String (fun x -> gw_dir := x), " Set gw folder.");
      ("-passwd", Arg.String (fun x -> passwd := x), " Set wizard password.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ( "-o",
        Arg.String (fun x -> out_file := x),
        " Name of the result (default family.gw2l)." );
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-f", Arg.Set no_fail, " Read the .gw file with no failinf");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-v", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
      ( "-test",
        Arg.Int
          (fun x ->
            test_nb := x;
            test := true),
        " Choose test file." );
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = base := s in
  Arg.parse speclist anonfun usage;

  Printf.eprintf
    "This is \027[32mmakeUpdImgl\027[0m version %s for family %s (%d) (%s)\n"
    Sutil.version !family !debug
    (if !verbose then "v" else "q");
  flush stderr;

  let in_file = String.concat Filename.dir_sep [ "tmp"; !family ^ ".tex" ] in
  let out_file = String.concat Filename.dir_sep [ "tmp"; !family ^ ".tmp" ] in

  let ic = open_in in_file in
  let oc = open_out out_file in

  if !debug = -1 then Sys.enable_runtime_warnings false;

  if !verbose then Printf.eprintf "Read images dictionnaries\n";
  flush stderr;
  (* read images dictionnaries *)
  let in_channel = open_in_bin "dict1.dat" in
  let dict1 = Marshal.from_channel in_channel in
  close_in in_channel;
  let in_channel = open_in_bin "dict2.dat" in
  let dict2 = Marshal.from_channel in_channel in
  close_in in_channel;
  let in_channel = open_in_bin "dict4.dat" in
  let dict4 = Marshal.from_channel in_channel in
  close_in in_channel;
  let in_channel = open_in_bin "list1.dat" in
  let img_ok_list = Marshal.from_channel in_channel in
  close_in in_channel;

  process ic oc dict1 dict2 dict4 img_ok_list;
  close_in ic;
  close_out oc

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
