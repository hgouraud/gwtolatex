(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

let basename = ref "chausey"
let family = ref ""
let livres = ref (try Sys.getenv "GW2L_LIVRES" with Not_found -> "./Livres")
let bases = ref (try Sys.getenv "GW2L_BASES" with Not_found -> "./")
let test = ref false
let out_file = ref ""
let dev = ref false
let verbose = ref false
let debug = ref 0
let test_nb = ref 0

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

let dict1 = Hashtbl.create 100

(* # uid;n;texte descriptif;nom-de-fichier.jpg *)
(* # voir ... *)
(* \index{X, Y}/z  ;z indique N° d'occurrence, "z" si ?? *)

let process base ic line =
  try
    let parts = String.split_on_char ';' line in
    if List.length parts <> 4 then
      Printf.eprintf "Bad image definition %s\n" line
    else if line = "" then ()
    else
      let uid = List.nth parts 0 in
      Printf.eprintf "Uid1: %s\n" uid;
      let anx_page = List.nth parts 1 in
      let desc = List.nth parts 2 in
      let fname = List.nth parts 3 in
      if Hashtbl.mem dict1 uid then
        Printf.eprintf "Duplicate image definition %s, %s\n" uid desc
      else
        let line = Sutil.input_real_line ic in
        let index_l =
          let rec loop line index_l =
            if line = "" then index_l
            else if Sutil.start_with "\\index" 0 line then
              let i = try String.index line '{' with Not_found -> -1 in
              let j = try String.index line '}' with Not_found -> -1 in
              if i <> -1 && j <> -1 then (
                let str = String.sub line (i + 1) (j - i - 1) in
                let ocn =
                  if String.length line > j + 2 then
                    String.sub line (j + 2) (String.length line - j - 2)
                  else ""
                in
                let parts = String.split_on_char ',' str in
                let sn =
                  if List.length parts > 0 then List.nth parts 0 else ""
                in
                let fn =
                  if List.length parts > 1 then List.nth parts 1 else ""
                in
                if ocn <> "" then Printf.eprintf "P: %s.%s %s\n" fn ocn sn;
                if sn = "" && fn = "" then loop (input_line ic) index_l
                else if ocn = "z" then
                  loop (input_line ic) ((fn, sn, 0) :: index_l)
                else
                  let fn, sn, ocn, _sp, _index_s =
                    Hutil.get_real_person base "" fn sn ocn
                  in
                  loop (input_line ic) ((fn, sn, ocn) :: index_l))
              else index_l
            else index_l
          in
          loop line []
        in
        Hashtbl.add dict1 uid (anx_page, desc, fname, index_l)
  with
  | Failure _ -> Printf.eprintf "Bad image definition %s\n" line
  | End_of_file -> ()

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> basename := x), " Choose base.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ( "-out",
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
  let start_time = Unix.gettimeofday () in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;

  let base = Hutil.open_base (Filename.concat "." !basename) in

  (* for my convenience. Win env may differ *)
  if
    Sys.argv.(0) = "_build/install/default/bin/mkImgDict"
    || Sys.argv.(0) = "_build\\install\\default\\bin\\mkImgDict.exe"
  then dev := true;

  if !verbose then
    for i = 0 to Array.length Sys.argv - 1 do
      Printf.printf "[%i] %s " i Sys.argv.(i)
    done;

  Printf.eprintf "\nThis is mkImgDict version %s for %s (%d)\n" Sutil.version
    !family !debug;
  flush stderr;

  let in_file =
    String.concat Filename.dir_sep
      [ !livres; !family ^ "-inputs"; "who_is_where.txt" ]
  in
  let out_file =
    String.concat Filename.dir_sep
      [ "."; "gw2l_dist"; "tmp"; !family ^ "-imgDict.tmp" ]
  in

  let ic = open_in in_file in
  let oc = open_out_bin out_file in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  try
    while true do
      let line = Sutil.input_real_line ic in
      process base ic line
    done
  with End_of_file ->
    Printf.eprintf "Done in %s s\n" (show_process_time start_time);
    close_in ic;
    close_out oc

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
