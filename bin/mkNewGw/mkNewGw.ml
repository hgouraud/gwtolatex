(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

(*
scan family.gw file producing family-new.gw
insert information in notes (create note if needed)
indication then pictures where the person is visible
Gets the data from the structure created by makeImgDict
*)

(* execution context *)
let base = ref "chausey"
let family = ref ""
let out_file = ref ""
let debug = ref 0
let dev = ref false
let verbose = ref false
let version = "1.0"

(* Assumes we are running in bases folder GeneWeb security constraint *)
let livres = ref (try Sys.getenv "GWTL_LIVRES" with Not_found -> "./Livres")
let bases = ref (try Sys.getenv "GWTL_BASES" with Not_found -> "./")
let test = ref false
let test_nb = ref 0

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
      ("-base", Arg.String (fun x -> base := x), " Which base.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ( "-o",
        Arg.String (fun x -> out_file := x),
        " Name of the result (default family.gw2l)." );
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
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
  let start_time = Unix.gettimeofday () in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = base := s in
  Arg.parse speclist anonfun usage;

  (* for my convenience. Win env may differ *)
  if
    Sys.argv.(0) = "_build/install/default/bin/gwl"
    || Sys.argv.(0) = "_build\\install\\default\\bin\\gwl.exe"
  then dev := true;

  if !verbose then
    for i = 0 to Array.length Sys.argv - 1 do
      Printf.printf "[%i] %s " i Sys.argv.(i)
    done;

  Printf.eprintf "\nThis is makeNewGw version %s for %s (%d)\n" version !base
    !debug;
  flush stderr;

  let in_file = String.concat Filename.dir_sep [ "."; !base ^ ".gw" ] in
  let out_file = String.concat Filename.dir_sep [ "."; !base ^ "-new.gw" ] in

  let ic = open_in in_file in
  let oc = open_out out_file in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  try
    while true do
      output_string oc (input_line ic ^ "\n")
    done
  with End_of_file ->
    Printf.eprintf "makeNewGw done in %s s\n" (show_process_time start_time);
    close_in ic;
    close_out oc

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
