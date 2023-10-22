(* Copyright (c) 2013 H.Gouraud *)

(* execution context *)
let base = ref "chausey"
let family = ref ""
let out_file = ref ""
let debug = ref 0
let mode = ref 0
let dev = ref false
let second = ref false
let index = ref 0
let verbose = ref false

(* Assumes we are running in bases folder GeneWeb security constraint *)
let livres = ref (try Sys.getenv "GWTL_LIVRES" with Not_found -> "./Livres")
let bases = ref (try Sys.getenv "GWTL_BASES" with Not_found -> "./")
let test = ref false
let follow = ref false
let test_nb = ref 0
let version = "1.0"

(*
Make a book for -family using data from -base with options
Families info are stored in -livres
Bases are in -bases

The process is decomposed as follows
- run makeImgDict to collect image information from family-inputs/group-index.tex (or txt)
- run gw/gwu on family to create family.gw
- run makeNewGw to create family-new.gw containing images reference data
- run gw/gwc on family-new.gw to create new temporary base
- run gw2l on family-new to create family.tex file
- run pdflatex on family.tex
- run tweekIndex on family.ind to adjust/merge index entries
- run pdflatex to obtain final version


# needs : a working base (Chausey-new), a txt file (Chausey.txt) storing requests to the base
# reads in Chausey-new-ImgDict1.pkl (created by pass 1, used by pass 2)
# reads in Chausey-new-ImgDict2.pkl (created from Chausey-inputs/index-groupes.txt)
#          Chausey-new-IndexDict.pkl
#          Chausey-inputs/index-groupes-out.txt (read in for addition into index)
# produces Chausey.pdf
# A-Make_genea-generic         Base        Family  nb-photos/page   index-group-photos
# Input: Chausey.txt
# Input: Nom-Prénoms.txt
# Input: Chausey-inputs/index-groupes-out.txt
# Input: Chausey-new-ImgDict2.pkl
# Input  (pass2): Chausey-new-ImgDict1.pkl
# Input: Cleanup files: Generic-cleanup.txt, Chausey-cleanup.txt
# Output (pass1): Chausey-new-ImgDict1.pkl
# Output: Chausey.tex
# Output: Chausey.pdf
# 
# Deux traitements supplémentaires sur l'index pour le compacter
# Make-tweek-index-sorting.py
# Make-tweek-index-merge.py
#

pdflatex   $MODE $FAMILY
makeindex $FAMILY
pdflatex  $MODE $FAMILY

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
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> base := x), " Choose base.");
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
      ("-second", Arg.Set second, " Run Pdflatex a second time.");
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-mode", Arg.Int (fun x -> mode := x), " Print tree mode.");
      ("-v", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
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

  (* for my convenience. Win env may differ *)
  if
    Sys.argv.(0) = "_build/install/default/bin/gwl"
    || Sys.argv.(0) = "_build\\install\\default\\bin\\gwl.exe"
  then dev := true;
  (* install tex templates in bases/etc *)
  (* on excute dans le repo (dev) ou dans bases_dir *)
  let dist_dir = if !dev then "." else "./gw2l_dist" in
  let etc_dir = Filename.concat !bases "etc" in
  let tex_dir = String.concat Filename.dir_sep [ dist_dir; "tex" ] in
  let do_load_tex_files = Format.sprintf "cp -R %s %s" tex_dir etc_dir in
  let error = Sys.command do_load_tex_files in
  if error > 0 then (
    Printf.eprintf "Error while loading tex templates files (%d)\n" error;
    exit 0);

  let _och = if !follow then open_out "fname_out" else stderr in
  let _ic = open_in_bin "fname_in" in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  Printf.eprintf "\nThis is makeBook version %s on %s to %s (%d)\n" version
    "fname_in" "fname_out" !debug;
  flush stderr;

  Printf.eprintf "Done txt parsing in %s s\n" (show_process_time start_time);
  flush stderr;

  let mode = if !verbose then "" else "-interaction=batchmode" in
  let fname =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; "family_out" ^ ".aux" ]
  in
  let do_rm_aux = Printf.sprintf "rm %s" fname in
  let do_pdflatex =
    Printf.sprintf "pdflatex -output-directory=%s %s %s"
      (String.concat Filename.dir_sep [ dist_dir; "tmp" ])
      mode "fname_out"
  in

  Printf.eprintf "First pass at pdflatex \n";
  flush stderr;
  let _ = Sys.command do_rm_aux in
  let error = Sys.command do_pdflatex in
  if error <> 0 then (
    Printf.eprintf "Error in pdflatex processing (%d)\n" error;
    Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
    exit 0);

  Printf.eprintf "Building index\n";
  flush stderr;
  if !test && !index = 0 then (
    Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
    exit 0);
  (* makeindex does not like absolute paths! *)
  let fname =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; "family_out" ^ ".idx" ]
  in
  let do_makeindex = Printf.sprintf "makeindex %s" fname in
  for _i = 0 to !index do
    let error = Sys.command do_makeindex in
    if error <> 0 then (
      Printf.eprintf "Error in makeindex processing (%d)\n" error;
      Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
      exit 0)
  done;

  if !second then (
    Printf.eprintf "Second pass at pdflatex \n";
    flush stderr;
    let error = Sys.command do_pdflatex in
    if error <> 0 then
      Printf.eprintf "Error in 2nd pdflatex processing (%d)\n" error;
    let error = Sys.command do_makeindex in
    if error <> 0 then
      Printf.eprintf "Error in makeindex processing (%d)\n" error);

  let fname = Filename.basename "fname_out" |> Filename.remove_extension in
  let pdf_name =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; fname ^ ".pdf" ]
  in
  let dir = if !dev then "test" else !livres in
  let do_move_pdf = Printf.sprintf "mv %s %s" pdf_name dir in
  let error = Sys.command do_move_pdf in
  if error <> 0 then Printf.eprintf "Error in moving pdf file (%d)\n" error;

  Printf.eprintf "Result file is in %s\n" (Filename.concat dir (fname ^ ".pdf"));
  Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
