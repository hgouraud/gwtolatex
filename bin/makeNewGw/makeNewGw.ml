(* Copyright (c) 2013 H.Gouraud *)

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

(* Assumes we are running in bases folder GeneWeb security constraint *)
let livres = ref (try Sys.getenv "GWTL_LIVRES" with Not_found -> "./Livres")
let bases = ref (try Sys.getenv "GWTL_BASES" with Not_found -> "./")
let test = ref false
let test_nb = ref 0

let _show_process_time start =
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
  let _start_time = Unix.gettimeofday () in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;

  (* for my convenience. Win env may differ *)
  if
    Sys.argv.(0) = "_build/install/default/bin/gwl"
    || Sys.argv.(0) = "_build\\install\\default\\bin\\gwl.exe"
  then dev := true



let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
