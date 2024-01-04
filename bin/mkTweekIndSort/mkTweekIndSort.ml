(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

let base = ref ""
let family = ref ""
let livres = ref (try Sys.getenv "GWTL_LIVRES" with Not_found -> "./Livres")
let bases = ref (try Sys.getenv "GWTL_BASES" with Not_found -> "./")
let test = ref false
let out_file = ref "./gw2l_dist/tmp/temp"
let dev = ref false
let verbose = ref false
let debug = ref 0
let test_nb = ref 0
let input_files = ref []

let show_process_time start =
  let process_time = Unix.gettimeofday () -. start in
  Format.sprintf "%.3f" process_time

(*
Read Livres/family-input/index-groups.tex
# n;texte descriptif;nom-de-fichier.jpg
# n = numero de page de l'annexe,
# ou 0 pour les photos dans le corps du document
# #voir nom, prénom/clé de la fiche personne où cette photo apparait
# clé devrait être fname.occ.lname!!
# #voir fiche personne où apparait cette photo
# plusieurs personnes possible
#\index -> index des personnes présentes sur la photo
#\index{X, Y}/z  ;z indique N° d'occurrence, "z" si ??
#
*)

(* POSIX lockf(3), and fcntl(2), releases its locks when the process
   that holds the locks closes ANY file descriptor that was open on that file.
*)
let _executable_magic =
  match Sys.getenv_opt "GW_EXECUTABLE_MAGIC" with
  | Some x -> x
  | None -> Digest.file Sys.executable_name

let _random_magic =
  Random.self_init ();
  Random.bits () |> string_of_int

let check_magic magic ic =
  let len = String.length magic in
  let pos = pos_in ic in
  if in_channel_length ic - pos < len then false
  else if magic = really_input_string ic len then true
  else (
    seek_in ic pos;
    false)

let read_or_create_channel ?magic ?(wait = false) fname read write =
  if Sys.os_type = "Win32" then (
    let _ = wait in
    ();
    assert (Secure.check fname);
    let fd = Unix.openfile fname [ Unix.O_RDWR; Unix.O_CREAT ] 0o666 in
    if Sys.os_type <> "Win32" then (
      try Unix.lockf fd (if wait then Unix.F_LOCK else Unix.F_TLOCK) 0
      with e ->
        Unix.close fd;
        raise e);
    let ic = Unix.in_channel_of_descr fd in
    let read () =
      seek_in ic 0;
      try
        match magic with
        | Some m when check_magic m ic ->
            let r = Some (read ic) in
            let _ = seek_in ic (in_channel_length ic - String.length m) in
            assert (check_magic m ic);
            r
        | Some _ -> None
        | None -> Some (read ic)
      with _ -> None
    in
    match read () with
    | Some v ->
        if Sys.os_type <> "Win32" then Unix.lockf fd Unix.F_ULOCK 0;
        close_in ic;
        v
    | None ->
        Unix.ftruncate fd 0;
        let oc = Unix.out_channel_of_descr fd in
        seek_out oc 0;
        (match magic with
        | Some m -> seek_out oc (String.length m)
        | None -> ());
        let v = write oc in
        flush oc;
        let _ = seek_out oc (out_channel_length oc) in
        (match magic with Some m -> output_string oc m | None -> ());
        (match magic with
        | Some m ->
            seek_out oc 0;
            output_string oc m
        | None -> ());
        flush oc;
        if Sys.os_type <> "Win32" then Unix.lockf fd Unix.F_ULOCK 0;
        close_out oc;
        v)

let _read_or_create_value ?magic ?wait fname create =
  let read ic = Marshal.from_channel ic in
  let write oc =
    let v = create () in
    Marshal.to_channel oc v [ Marshal.No_sharing; Marshal.Closures ];
    v
  in
  try read_or_create_channel ?magic ?wait fname read write with _ -> create ()

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
        " Name of the result (default family.idct)." );
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
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = input_files := s :: !input_files in
  Arg.parse speclist anonfun usage;

  let in_file =
    String.concat Filename.dir_sep [ "."; "gw2l_dist"; "tmp"; !family ^ ".idx" ]
  in

  Printf.eprintf "This is \027[32mTweekIndSort\027[0m version %s on %s to %s (%d)\n"
    Sutil.version in_file !out_file !debug;
  flush stderr;

  let ic = open_in in_file in
  let oc = open_out !out_file in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  try
    while true do
      let line = input_line ic in
      output_string oc (line ^ "\n")
    done
  with End_of_file ->
    Printf.eprintf "\n";
    close_in ic;
    close_out oc

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
