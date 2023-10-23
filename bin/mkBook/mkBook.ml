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
let gw_dir = ref (try Sys.getenv "GW_DIR" with Not_found -> "./")
let test = ref false
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

  Printf.eprintf "\nThis is mkBook version %s on base %s for family %s (%d)\n"
    version !base !family !debug;
  flush stderr;

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

  let tmp = String.concat Filename.dir_sep [ "."; "tmp" ] in
  if not (Sys.file_exists tmp) then (
    try
      let _ = Sys.command (Format.sprintf "mkdir %s 755" tmp) in
      ()
    with Sys_error _ ->
      Printf.eprintf "Failed to create tmp dir\n";

      (* Create family.gw file *)
      let gwu = Filename.concat !gw_dir "gwu" in
      let out = Filename.concat !bases !base ^ ".gw" in
      let make_gw_file = Format.sprintf "%s -o %s %s" gwu out !base in
      let error = Sys.command make_gw_file in
      if error > 0 then (
        Printf.eprintf "Error while creating .gw file (%d)\n" error;
        exit 0);

      (* rm imgDict ?? *)

      (* run mkImgDict *)
      let in_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !base ^ ".gw" ]
      in
      let out_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !base ^ "-new.gw" ]
      in
      let make_imgDct =
        Format.sprintf "mkImgDict -in %s -o %s" in_file out_file
      in
      let error = Sys.command make_imgDct in
      if error > 0 then (
        Printf.eprintf "Error while creating imgDict (%d)\n" error;
        exit 0);

      (* make new base *)
      let gwc = Filename.concat !gw_dir "gwc" in
      let in_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !base ^ "-new.gw" ]
      in
      let out_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !base ^ ".gwb" ]
      in
      let log_file = String.concat Filename.dir_sep [ "."; "tmp"; "gwc.log" ] in
      let make_new_base =
        Format.sprintf "%s -f -o %s %s > %s" gwc out_file in_file log_file
      in
      let error = Sys.command make_new_base in
      if error > 0 then (
        Printf.eprintf "Error while creating new base (%d)\n" error;
        exit 0);

      (* run gwl *)
      (* output and aux files are in ./tmp  (mkdir ./tmp if needed) *)
      let gwl = String.concat Filename.dir_sep [ "."; "gw2l_dir"; "gwl" ] in
      let make_tex_file =
        Format.sprintf "%s -base %s -family %s -livres %s" gwl (!base ^ "-new")
          !family !livres
      in
      let error = Sys.command make_tex_file in
      if error > 0 then (
        Printf.eprintf "Error while creating .tex file (%d)\n" error;
        exit 0);

      (* run pdflatex *)
      let aux_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !family ^ "-new.aux" ]
      in
      let do_rm_aux = Printf.sprintf "rm %s" aux_file in
      let _ = Sys.command do_rm_aux in
      let mode = if !verbose then "" else "-interaction=batchmode" in
      let tex_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !family ^ "-new.tex" ]
      in
      let make_pdf_file =
        Printf.sprintf "pdflatex -output-directory=%s %s %s"
          (String.concat Filename.dir_sep [ "."; "tmp" ])
          mode tex_file
      in
      let error = Sys.command make_pdf_file in
      if error > 0 then (
        Printf.eprintf "Error while creating .pdf file (%d)\n" error;
        exit 0);

      (* run makeindex *)
      let idx_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !family ^ "-new.idx" ]
      in
      (* makeindex does not like absolute paths! *)
      let make_index = Printf.sprintf "makeindex %s" idx_file in
      let error = Sys.command make_index in
      if error > 0 then (
        Printf.eprintf "Error while creating index file (%d)\n" error;
        exit 0);

      (* run mkTweekInd *)
      let ind_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !family ^ "-new.ind" ]
      in
      let ind_file_tmp =
        String.concat Filename.dir_sep [ "."; "tmp"; !family ^ "-new.ind.tmp" ]
      in
      (* makeindex does not like absolute paths! *)
      let move_index = Printf.sprintf "mv %s %s" ind_file ind_file_tmp in
      let tweek_index = Printf.sprintf "mkTweekInd %s" ind_file_tmp in
      let _ = Sys.command move_index in
      let error = Sys.command tweek_index in
      if error > 0 then (
        Printf.eprintf "Error while merging index file (%d)\n" error;
        exit 0);

      (* run pdflatex second time *)
      let error = Sys.command make_pdf_file in
      if error > 0 then (
        Printf.eprintf "Error while second pdflatex run (%d)\n" error;
        exit 0);

      (* move pdf to livres *)
      let pdf_file =
        String.concat Filename.dir_sep [ "."; "tmp"; !family ^ "-new.pdf" ]
      in
      let final_pdf_file =
        String.concat Filename.dir_sep [ !livres; !family ^ ".pdf" ]
      in
      let do_mv_pdf = Printf.sprintf "mv %s %s" pdf_file final_pdf_file in
      let error = Sys.command do_mv_pdf in
      if error > 0 then (
        Printf.eprintf "Error while moving pdf file(%d)\n" error;
        exit 0);

      Printf.eprintf "Result file is in %s\n" final_pdf_file;
      Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
      flush stderr)

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
