(* Copyright (c) 2024 H.Gouraud *)
open Gwtolatex

(* execution context *)
let dist_dir = ref "./gw2l_dist"
let base = ref ""
let passwd = ref ""
let family = ref ""
let out_file = ref ""
let debug = ref 0
let mode = ref 0
let dev = ref false
let second = ref true
let index = ref 0
let verbose = ref false

(* Assumes we are running in bases folder because of GeneWeb security constraint *)
let livres = ref "../livres"
let bases = ref "."
let gw_dir = ref (try Sys.getenv "GW_BIN" with Not_found -> "./")
let gw2l_dir = ref "./gw2l_dist"
let test = ref false
let test_nb = ref 0

(*
Make a book for -family using data from -base with gw2l_options
Families info are stored in -livres
Bases are in -bases

The process is decomposed as follows
- run gw/gwu on base to create base.gw
- run gw2l/mkNewGw to create base-new.gw containing images reference data
- run gw/gwc on base-new.gw to create new temporary base
- run gw2l/mkTex on base-new and family to create family.tex file
- run pdflatex on family.tex
- run tweekIndex on family.ind to adjust/merge index entries
- run pdflatex to obtain final version

(from the old Python solution)
Make-tweek-index-sorting.py $BASE $FAMILY
mv ./$FAMILY.idx ./$FAMILY-idx.sav
mv -f ./$FAMILY-out.idx ./$FAMILY.idx
makeindex $FAMILY
Make-tweek-index-merge.py $BASE $FAMILY NoPhotos
mv ./$FAMILY.ind ./$FAMILY-ind.sav
mv -f ./$FAMILY-out.ind ./$FAMILY.ind
pdflatex  $MODE $FAMILY

*)

let show_process_time start =
  let process_time = Unix.gettimeofday () -. start in
  Format.sprintf "%.3f" process_time

let main () =
  let usage =
    "Usage: "
    ^ Filename.basename Sys.argv.(0)
    ^ " [gw2l_options] where gw2l_options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> base := x), " Choose base.");
      ("-passwd", Arg.String (fun x -> passwd := x), " Set wizard password.");
      ("-gw", Arg.String (fun x -> gw_dir := x), " Set gw folder.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ( "-o",
        Arg.String (fun x -> out_file := x),
        " Name of the result (default family.gw2l)." );
      ( "-index",
        Arg.Int (fun x -> index := x),
        " Number of times makeindex is done." );
      ("-once", Arg.Clear second, " Do not run Pdflatex a second time.");
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-mode", Arg.Int (fun x -> mode := x), " Print tree mode.");
      ("-v", Arg.Set verbose, " verbose or quiet.");
      ( "-test",
        Arg.Int
          (fun x ->
            test_nb := x;
            test := true),
        " Choose test file." );
    ]
  in
  let start_time = Unix.gettimeofday () in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;

  let base_new = !base ^ "-new" in
  Printf.eprintf
    "\nThis is \027[32mmkBook\027[0m version %s on base %s for family %s (%d)\n"
    Sutil.version !base !family !debug;
  flush stderr;

  let print_chan channel =
    let rec loop () =
      let () = print_endline (input_line channel) in
      loop ()
    in
    try loop () with End_of_file -> close_in channel
  in

  let _exec cmd =
    let ocaml_stdout, ocaml_stdin, ocaml_stderr =
      Unix.open_process_full cmd [||]
    in
    close_out ocaml_stdin;
    print_chan ocaml_stdout;
    print_chan ocaml_stderr
  in

  let _string_of_command () =
    let tmp_file = Filename.temp_file "" ".txt" in
    let _ = Sys.command @@ "minisat test.txt | grep 'SATIS' >" ^ tmp_file in
    let chan = open_in tmp_file in
    let s = input_line chan in
    close_in chan;
    s
  in

  (* install tex templates in bases/etc *)
  (* on excute dans le repo (dev) ou dans bases_dir *)
  if !verbose then Printf.eprintf "Installing TeX templates\n";
  let etc_dir = Filename.concat !bases "etc" in
  let tex_dir = String.concat Filename.dir_sep [ !dist_dir; "tex" ] in
  let do_load_tex_files = Format.sprintf "cp -R %s %s" tex_dir etc_dir in
  let error = Sys.command do_load_tex_files in
  if error > 0 then (
    Printf.eprintf "Error while loading tex templates files (%d)\n" error;
    exit 0);
  flush stderr;

  if !verbose then Printf.eprintf "Create temp dir\n";
  let tmp = String.concat Filename.dir_sep [ "."; "tmp" ] in
  (if not (Sys.file_exists tmp) then
   try
     let _ = Sys.command (Format.sprintf "mkdir %s" tmp) in
     ()
   with Sys_error _ -> Printf.eprintf "Failed to create tmp dir\n");
  flush stderr;

  (* collect options *)
  let gw2l_options = [] in
  let gw2l_options =
    if !livres <> "" then Format.sprintf "-livres %s" !livres :: gw2l_options
    else gw2l_options
  in
  let gw2l_options =
    if !family <> "" then Format.sprintf "-family %s" !family :: gw2l_options
    else gw2l_options
  in
  let gw2l_options =
    if !base <> "" then Format.sprintf "-base %s" !base :: gw2l_options
    else gw2l_options
  in
  let gw2l_options =
    if !gw_dir <> "" then Format.sprintf "-gw %s" !gw_dir :: gw2l_options
    else gw2l_options
  in
  let gw2l_options =
    if !passwd <> "" then Format.sprintf "-passwd %s" !passwd :: gw2l_options
    else gw2l_options
  in
  let gw2l_options =
    if !debug <> 0 then Format.sprintf "-debug %d" !debug :: gw2l_options
    else gw2l_options
  in
  let gw2l_options =
    if !mode <> 0 then Format.sprintf "-mode %d" !mode :: gw2l_options
    else gw2l_options
  in
  let gw2l_options = if !verbose then "-v" :: gw2l_options else gw2l_options in
  let gw2l_options = String.concat " " gw2l_options in

  (* Create base.gw file *)
  if !verbose then Printf.eprintf "Create %s.gw file\n" !base;
  let gwu = Filename.concat !gw_dir "gwu" in
  let out = Filename.concat "." !base ^ ".gw" in
  let make_gw_file = Format.sprintf "%s -o %s %s" gwu out !base in
  if !verbose then Printf.eprintf "Commd: %s\n" make_gw_file;
  flush stderr;
  let error = Sys.command make_gw_file in
  if error > 0 then (
    Printf.eprintf "Error while creating .gw file (%d)\n" error;
    exit 0);
  flush stderr;

  (* make new .gw file *)
  if !verbose then Printf.eprintf "Make new %s-new.gw file\n" !base;
  let make_new_gw = Filename.concat !dist_dir "mkNewGw" in
  let make_new_gw_file = Format.sprintf "%s %s\n" make_new_gw gw2l_options in
  if !verbose then Printf.eprintf "Commd: %s\n" make_new_gw_file;
  flush stderr;
  let error = Sys.command make_new_gw_file in
  if error > 0 then (
    Printf.eprintf "Error while creating new gw file (%d)\n\n" error;
    exit 0);
  flush stderr;

  (* make new base from base-new.gw *)
  if !verbose then Printf.eprintf "Make new base: %s-new.gw\n" !base;
  flush stderr;
  let gwc = Filename.concat !gw_dir "gwc" in
  let in_file = String.concat Filename.dir_sep [ "."; base_new ^ ".gw" ] in
  let log_file = String.concat Filename.dir_sep [ "."; "tmp"; "gwc.log" ] in
  let make_new_base =
    Format.sprintf "%s -f -o %s %s > %s" gwc base_new in_file log_file
  in
  if !verbose then Printf.eprintf "Commd: %s\n" make_new_base;
  flush stderr;
  let error = Sys.command make_new_base in
  if error > 0 then (
    Printf.eprintf
      {|Error while creating new base (%d)
Inspect %s/tmp/gwc.log for possible errors.|}
      error !bases;
    exit 0);
  flush stderr;

  (* run mkTex *)
  (* output and aux files are in ./tmp  (mkdir ./tmp if needed) *)
  if !verbose then Printf.eprintf "Run MkTeX\n";
  flush stderr;
  let mkTex_exe = Filename.concat !gw2l_dir "mkTex" in
  let gw2l_options =
    Str.global_replace
      (Str.regexp ("-base " ^ !base))
      ("-base " ^ !base ^ "-new")
      gw2l_options
  in
  let out_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".tex" ]
  in
  let make_tex_file =
    Format.sprintf "%s -o %s %s" mkTex_exe out_file gw2l_options
  in
  if !verbose then Printf.eprintf "Commd: %s\n" make_tex_file;
  flush stderr;
  let error = Sys.command make_tex_file in
  if error > 0 then (
    Printf.eprintf "Error while creating .tex file (%d)\n" error;
    exit 0);
  flush stderr;

  (* run pdflatex *)
  if !verbose then Printf.eprintf "Run pdflatex\n";
  let aux_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".aux" ]
  in
  let do_rm_aux = Printf.sprintf "rm %s" aux_file in
  let _ = Sys.command do_rm_aux in
  let mode = if !verbose then "" else "-interaction=batchmode" in
  let make_pdf_file =
    Printf.sprintf "pdflatex -output-directory=%s %s %s"
      (String.concat Filename.dir_sep [ "."; "tmp" ])
      mode (!family ^ ".tex")
  in
  if !verbose then Printf.eprintf "Commd: %s\n" make_pdf_file;
  flush stderr;
  let error = Sys.command make_pdf_file in
  if error > 0 then (
    Printf.eprintf "Error while creating .pdf file (%d)\n\n" error;
    exit 0);
  flush stderr;

  (* might be necessary to process .idx before makeindex *)

  (* run makeindex *)
  if !verbose then Printf.eprintf "Run makeindex\n";
  let idx_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".idx" ]
  in
  (* makeindex does not like absolute paths! *)
  let make_index = Printf.sprintf "makeindex %s" idx_file in
  if !verbose then Printf.eprintf "Commd: %s\n" make_index;
  flush stderr;
  let error = Sys.command make_index in
  if error > 0 then (
    Printf.eprintf "Error while creating index file (%d)\n\n" error;
    exit 0);
  flush stderr;

  (* run mkTweekInd *)
  if !verbose then Printf.eprintf "Run mkTweekInd\n";
  let tweek_index_exe = Filename.concat !gw2l_dir "mkTweekInd" in
  (* makeindex does not like absolute paths! *)
  let tweek_index = Printf.sprintf "%s -family %s" tweek_index_exe !family in
  if !verbose then Printf.eprintf "Commd: %s\n" tweek_index;
  flush stderr;
  let error = Sys.command tweek_index in
  if error > 0 then (
    Printf.eprintf "Error while managing index file (%d)\n\n" error;
    exit 0);
  flush stderr;

  (* run pdflatex second time *)
  if !second then (
    if !verbose then Printf.eprintf "Run pdflatex second time\n";
    flush stderr;
    let error = Sys.command make_pdf_file in
    if error > 0 then (
      Printf.eprintf "Error during second pdflatex run (%d)\n\n" error;
      exit 0);
    flush stderr);

  (* insert annexe *)
  let annex_file =
    String.concat Filename.dir_sep [ !livres; !family ^ "-inputs"; "Annexes.pdf" ]
  in
  if Sys.file_exists annex_file then (
    if !verbose then Printf.eprintf "Append annex\n";
    let _ =
      Sys.command
        (Format.sprintf "cp %s %s" annex_file (Filename.concat "." "tmp"))
    in
    let annex_cmd =
      Format.sprintf "%s %s"
        (Filename.concat !gw2l_dir "append-annex.sh")
        !family
    in
    flush stderr;
    let error = Sys.command annex_cmd in
    if error > 0 then (
      Printf.eprintf "Error while appending annex (%d)\n\n" error;
      exit 0);
    flush stderr);

  (* move pdf to livres *)
  if !verbose then Printf.eprintf "Move .pdf file to Livres\n";
  let pdf_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".pdf" ]
  in
  let final_pdf_file =
    String.concat Filename.dir_sep [ !livres; !family ^ ".pdf" ]
  in
  let do_mv_pdf = Printf.sprintf "mv %s %s" pdf_file final_pdf_file in
  let error = Sys.command do_mv_pdf in
  if error > 0 then (
    Printf.eprintf "Error while moving pdf file(%d)\n\n" error;
    exit 0);
  flush stderr;

  Printf.eprintf "Result file is in %s\n" final_pdf_file;
  Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
