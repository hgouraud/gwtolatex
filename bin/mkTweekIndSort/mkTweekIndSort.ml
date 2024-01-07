(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

let family = ref ""
let dev = ref false
let verbose = ref false
let debug = ref 0
let input_files = ref []

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


type item = {
  surname : string;
  first_name : string;
  photo_id : string;
  numbers : string list;
}

(*
, , , 
\begin{theindex}, , , 
, , , 
\indexspace, , , 
, , , 
\item Marie, Alain, 42, 177, 65, 56
\item Marie, Albert, 45, 184, 180, 178, 176, 173, 172, 89, 87, 62, 56, 186, 187, 226, 232, 234, 235, 330
\item Marie, Albert (1), 51, 175, 174, 167, 161, 146, 107, 68, 56, 181, 182, 187, 205, 215, 219, 226, 234, 235, 278, 281, 305, 328, 330, 348
\item Marie, Albert (Megot), 174, 
\item Marie, Albert (Mégot), 11, 339, 225
\item Marie, Albert (Toumagi), 172, 352, 351, 227
\item Marie, Albert.1, présent sur photo 101, 181, 176
\item Marie, Albert.1, présent sur photo 126, 220, 183, 176
\item Marie, Albert.1, présent sur photo 19 en annexe page 16, 167
\item Marie, Albert.1, présent sur photo 209, 147
\item Marie, Albert.1, présent sur photo 210, 216, 183, 176
, , , 
\end{theindex}, , , 
*)

let merge_index_file items =
  let entries = Hashtbl.create 100 in
  let loop items =
    match items with
    | [] -> entries
    | item :: items with item.surname = "" -> loop items
    | item :: items with item.surname = "\\indexspace" -> loop items
    | item :: items with Sutil.start_with "\\item" 0 item.surname ->
        let surname = String.sub item.surname 6 (String.length item.surname) in
        let first_name = item.first_name in
        
      
        loop items
    | item :: items ->
        (Printf.eprintf "Bad idem %s\n" item.surname;
        loop items)
  in loop [] items
  


(*
\begin{theindex}x

  \item Abbé Delaby, voir Delaby, André, 26, 97, 103, 151, 164, 264, 
		266, 293, 305, 346, 348, 349

  \indexspace

  \item Marie, Albert, 45, 56, 62, 87, 89, 172, 173, 176, 178, 180, 184, 
		186, 187, 226, 232, 234, 235, 330
  \item Marie, Albert (1), 51, 56, 68, 107, 146, 161, 167, 174, 175, 
		181, 182, 187, 205, 215, 219, 226, 234, 235, 278, 281, 
		305, 328, 330, 348
  \item Marie, Albert (Megot), 174
  \item Marie, Albert (Mégot), 11, 225, 339
  \item Marie, Albert (Toumagi), 172, 227, 351, 352
  \item Marie, Albert.1, présent sur photo 101, 176, 181
  \item Marie, Albert.1, présent sur photo 19 en annexe page 16, 167

  \indexspace

  \item Île d'Aneret, 261, 318, 319
  \item Îles Chausey, 200

\end{theindex}
*)

(** Function to parse a line and create an item *)
let parse_line line =
  let line =
    if String.length line > 0 && line.[String.length line - 1] = ',' then
      String.sub line 0 (String.length line - 1)
    else line
  in
  let parts = String.split_on_char ',' line in
  match List.length parts with
  | 0 -> { surname = ""; first_name = ""; photo_id = ""; numbers = [] }
  | 1 ->
      {
        surname = List.nth parts 0 |> String.trim;
        first_name = "";
        photo_id = "";
        numbers = [];
      }
  | 2 ->
      {
        surname = List.nth parts 0 |> String.trim;
        first_name = List.nth parts 1 |> String.trim;
        photo_id = "";
        numbers = [];
      }
  | 3 ->
      {
        surname = List.nth parts 0 |> String.trim;
        first_name = List.nth parts 1 |> String.trim;
        photo_id = List.nth parts 2 |> String.trim;
        numbers = [];
      }
  | _ ->
      {
        surname = List.nth parts 0 |> String.trim;
        first_name = List.nth parts 1 |> String.trim;
        photo_id = List.nth parts 2 |> String.trim;
        numbers =
          (let rec loop i acc =
             if i = List.length parts then acc
             else
               let nbr = String.trim (List.nth parts i) in
               if nbr <> "" then loop (i + 1) (nbr :: acc) else loop (i + 1) acc
           in
           loop 3 []);
      }

(** Function to read the index file and obtain the list of items *)
let read_index_file ic =
  let rec read_items acc current_item =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = '\t' then (
        let line = String.trim line in
        let line =
          if String.length line > 0 && line.[String.length line - 1] = ',' then
            String.sub line 0 (String.length line - 1)
          else line
        in
        let numbers = List.map String.trim (String.split_on_char ',' line) in
        Printf.eprintf "Supp line: (%d)+(%d) %s\n"
          (List.length current_item.numbers)
          (List.length numbers) line;
        let updated_item =
          { current_item with numbers = current_item.numbers @ numbers }
        in
        read_items acc updated_item)
      else
        let item = parse_line (String.trim line) in
        read_items (current_item :: acc) item
    with End_of_file -> List.rev (current_item :: acc)
  in
  read_items [] { surname = ""; first_name = ""; photo_id = ""; numbers = [] }

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-v", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = input_files := s :: !input_files in
  Arg.parse speclist anonfun usage;

  let in_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".ind" ]
  in
  let out_file = String.concat Filename.dir_sep [ "."; "tmp"; "temp" ] in

  Printf.eprintf
    "This is \027[32mmkTweekIndSort\027[0m version %s on %s to %s (%d)\n"
    Sutil.version in_file out_file !debug;
  flush stderr;

  let ic = open_in in_file in
  let oc = open_out out_file in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  let items = read_index_file ic in
  
  let items = merge_index_file items in
  
  List.iter
    (fun item ->
      output_string oc
        (Format.sprintf "%s, %s, %s, %s\n" item.surname item.first_name
           item.photo_id
           (String.concat ", " item.numbers)))
    items;

  close_in ic;
  close_out oc

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
