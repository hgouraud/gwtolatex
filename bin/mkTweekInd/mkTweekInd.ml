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

let _print_item (item : item) =
  Printf.eprintf "Item: %s, %s, %s, %s\n" item.surname item.first_name
    item.photo_id
    (String.concat ", " item.numbers)

type entry = {
  surname : string;
  first_name : string;
  photo_id : string;
  numbers_main : string list;
  numbers_photos : string list;
  numbers_annex : string list;
}

let _print_entry (entry : entry) =
  Printf.eprintf "Entry: %s, %s, %s, %s\n" entry.surname entry.first_name
    entry.photo_id
    (Format.sprintf "main: %s, photos: %s, annex: %s"
       (String.concat ", " entry.numbers_main)
       (String.concat ", " entry.numbers_photos)
       (String.concat ", " entry.numbers_annex))

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
\item Marie, Albert (1), photo 101, 181, 176
\item Marie, Albert (1), photo 126, 220, 183, 176
\item Marie, Albert (1), photo 19/16, 167
\item Marie, Albert (1), photo 209, 147
\item Marie, Albert (1), photo 210, 216, 183, 176
, , , 
\end{theindex}, , , 
*)

let merge_index_file items =
  let entries = Hashtbl.create 100 in
  let entries =
    let rec loop items =
      match items with
      | [] -> entries
      | (item : item) :: items when item.surname = "" -> loop items
      | (item : item) :: items when item.surname = "\\indexspace" -> loop items
      | (item : item) :: items when Sutil.start_with "\\item" 0 item.surname
        -> (
          let surname =
            String.sub item.surname 6 (String.length item.surname - 6)
          in
          let first_name = item.first_name in
          let photo_id = item.photo_id in
          let annex = String.contains photo_id '/' in
          let numbers = item.numbers in
          let key = surname ^ first_name in
          let numbers =
            if photo_id <> "" then
              List.map (fun p -> photo_id ^ "/" ^ p) numbers
            else numbers
          in
          match Hashtbl.find_opt entries key with
          | Some entry ->
              if photo_id = "" then
                Hashtbl.replace entries key
                  { entry with numbers_main = entry.numbers_main @ numbers }
              else if annex then
                Hashtbl.replace entries key
                  { entry with numbers_annex = entry.numbers_annex @ numbers }
              else
                Hashtbl.replace entries key
                  { entry with numbers_photos = entry.numbers_photos @ numbers };
              loop items
          | None ->
              if photo_id = "" then
                Hashtbl.add entries key
                  {
                    surname = item.surname;
                    first_name = item.first_name;
                    photo_id = "";
                    numbers_main = numbers;
                    numbers_photos = [];
                    numbers_annex = [];
                  }
              else if annex then
                Hashtbl.add entries key
                  {
                    surname = item.surname;
                    first_name = item.first_name;
                    photo_id = "";
                    numbers_main = [];
                    numbers_photos = [];
                    numbers_annex = numbers;
                  }
              else
                Hashtbl.add entries key
                  {
                    surname = item.surname;
                    first_name = item.first_name;
                    photo_id = "";
                    numbers_main = [];
                    numbers_photos = numbers;
                    numbers_annex = [];
                  };
              loop items)
      | (_item : item) :: items -> loop items
    in
    loop items
  in
  entries

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
  \item Marie, Albert (1), photo 101, 176, 181
  \item Marie, Albert (1), photo 19/16, 167

  \indexspace

  \item Île d'Aneret, 261, 318, 319
  \item Îles Chausey, 200

\end{theindex}
*)

let empty_item = { surname = ""; first_name = ""; photo_id = ""; numbers = [] }

(* \item Abbé Delaby, voir Delaby, André, 26, 97, 103, 151, 164, 264, *)

(** Function to parse a line and create an item *)
let parse_line line =
  let line =
    if String.length line > 0 && line.[String.length line - 1] = ',' then
      String.sub line 0 (String.length line - 1)
    else line
  in
  let parts = String.split_on_char ',' line in
  let parts = List.map String.trim parts in
  let get_first_name limit =
    let rec loop (i, acc) =
      if i = List.length parts then acc
      else if i < limit then loop (i + 1, List.nth parts i :: acc)
      else acc
    in
    loop (1, []) |> List.rev |> String.concat ", "
  in
  let rec read_page_nbrs i acc =
    if i = List.length parts then acc
    else
      let nbr = String.trim (List.nth parts i) in
      if nbr <> "" then read_page_nbrs (i + 1) (nbr :: acc)
      else read_page_nbrs (i + 1) acc
  in
  let get_photo_id_numbers numbers =
    let first = List.nth numbers 0 in
    if Mutil.contains first "photo " then
      (String.sub first 6 (String.length first - 6), List.tl numbers)
    else ("", numbers)
  in
  let rec find_index_of_string lst target index =
    match lst with
    | [] -> -1 (* Indicates that the target string was not found *)
    | head :: tail ->
        if Mutil.contains head target then index
          (* Return the current index if the target string is found *)
        else find_index_of_string tail target (index + 1)
  in
  (* FIXME Will fail with 12/34 and 12--34 *)
  let is_integer_string s =
    try
      ignore (int_of_string s);
      true
    with Failure _ -> false
  in
  let rec find_index_of_integer_string lst index =
    match lst with
    | [] -> -1 (* Indicates that no integer string was found *)
    | head :: tail ->
        if is_integer_string head then index
          (* Return the current index if an integer string is found *)
        else find_index_of_integer_string tail (index + 1)
  in
  let photo_indx = find_index_of_string parts "photo" 0 in
  let integer_indx = find_index_of_integer_string parts 0 in
  match photo_indx with
  | -1 -> (
      match integer_indx with
      | -1 | 0 -> empty_item
      | 1 ->
          {
            surname = List.nth parts 0 |> String.trim;
            first_name = "";
            photo_id = "";
            numbers = List.tl parts;
          }
      | _ ->
          let first_name = get_first_name integer_indx in
          let numbers = read_page_nbrs integer_indx [] |> List.rev in
          let photo_id, numbers = get_photo_id_numbers numbers in
          {
            surname = List.nth parts 0 |> String.trim;
            first_name;
            photo_id;
            numbers;
          })
  | _ -> (
      match List.length parts with
      | 2 ->
          let numbers = read_page_nbrs photo_indx [] |> List.rev in
          let photo_id, numbers = get_photo_id_numbers numbers in
          {
            surname = List.nth parts 0 |> String.trim;
            first_name = "";
            photo_id;
            numbers;
          }
      | _ ->
          let numbers = read_page_nbrs photo_indx [] |> List.rev in
          let photo_id, numbers = get_photo_id_numbers numbers in
          (* parts elements after surname and before photo_index *)
          let first_name = get_first_name photo_indx in
          {
            surname = List.nth parts 0 |> String.trim;
            first_name;
            photo_id;
            numbers;
          })

(** Function to read the index file and obtain the list of items *)
let read_index_file ic =
  let rec read_items acc (current_item : item) =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = '\t' then
        let line = String.trim line in
        let line =
          if String.length line > 0 && line.[String.length line - 1] = ',' then
            String.sub line 0 (String.length line - 1)
          else line
        in
        let numbers = List.map String.trim (String.split_on_char ',' line) in
        let updated_item =
          { current_item with numbers = current_item.numbers @ numbers }
        in
        read_items acc updated_item
      else
        let item = parse_line (String.trim line) in
        read_items (current_item :: acc) item
    with
    | End_of_file -> List.rev (current_item :: acc)
    | Failure line ->
        Printf.eprintf "Failed line: %s\n" line;
        read_items (current_item :: acc)
          { surname = ""; first_name = ""; photo_id = ""; numbers = [] }
  in
  read_items [] empty_item

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
  let out_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".ind.tmp" ]
  in
  let sav_file =
    String.concat Filename.dir_sep [ "."; "tmp"; !family ^ ".ind.sav" ]
  in

  (* if not (Sys.file_exists in_file) then exit 0; *)
  let ic = open_in in_file in

  Printf.eprintf
    "This is \027[32mmkTweekInd\027[0m version %s on %s to %s (%d)\n"
    Sutil.version in_file out_file !debug;
  flush stderr;

  let copy_index = Printf.sprintf "cp %s %s" in_file sav_file in
  let _ = Sys.command copy_index in

  let oc = open_out out_file in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  let items = read_index_file ic in

  let entries = merge_index_file items in

  let new_entries =
    Hashtbl.fold
      (fun _key (entry : entry) acc ->
        let numbers =
          let main =
            if List.length entry.numbers_main > 0 then
              let plural =
                if List.length entry.numbers_main > 1 then "s" else ""
              in
              Format.sprintf " page%s %s" plural
                (String.concat ", " entry.numbers_main)
            else ""
          in
          let photos =
            if List.length entry.numbers_photos > 0 then
              let photos =
                List.fold_left
                  (fun acc nbr ->
                    let nbr = String.split_on_char '/' nbr in
                    (List.nth nbr 0, List.nth nbr 1) :: acc)
                  [] entry.numbers_photos
                |> List.sort compare
              in
              let photos =
                let rec loop prev acc p_acc photos =
                  match photos with
                  | [] ->
                      if p_acc <> [] then (prev, List.rev p_acc) :: acc
                      else acc |> List.rev
                  | (n, p) :: photos ->
                      if n <> prev then
                        loop n ((n, p :: p_acc) :: acc) [] photos
                      else loop n acc (p :: p_acc) photos
                in
                loop "" [] [] photos |> List.rev
              in
              let photos =
                List.map
                  (fun (n, p_l) ->
                    let plural = if List.length p_l > 1 then "s" else "" in
                    Format.sprintf " photo %s page%s %s" n plural
                      (String.concat ", " p_l))
                  photos
              in
              Format.sprintf "%s" (String.concat ", " photos)
            else ""
          in
          let et =
            if
              List.length entry.numbers_main > 0
              || List.length entry.numbers_photos > 0
            then "et "
            else " "
          in
          let annex =
            if List.length entry.numbers_annex > 0 then
              let photos_annex =
                List.fold_left
                  (fun acc nbr ->
                    let nbr = String.split_on_char '/' nbr in
                    Format.sprintf "photo %s page %s" (List.nth nbr 0)
                      (List.nth nbr 1)
                    :: acc)
                  [] entry.numbers_annex
              in
              Format.sprintf "%sen annexe %s" et
                (String.concat ", " (List.rev photos_annex))
            else ""
          in
          let numbers =
            [ main; photos; annex ]
            |> List.filter (fun x -> x <> "")
            |> String.concat ", "
          in
          let numbers = Sutil.replace_str numbers ", et" " et" in
          Format.sprintf "%s" numbers
        in
        (* TODO filter page numbers across several lists avoiding duplicates *)
        (entry.surname, entry.first_name, numbers) :: acc)
      entries []
  in

  let new_entries =
    List.sort
      (fun (surname1, first_name1, _) (surname2, first_name2, _) ->
        let sn1 = Name.lower surname1 in
        let sn2 = Name.lower surname2 in
        let fn1 = Name.lower first_name1 in
        let fn2 = Name.lower first_name2 in
        if sn1 = sn2 then compare fn1 fn2 else compare sn1 sn2)
      new_entries
  in

  let prev_head = ref ' ' in
  output_string oc "\\begin{theindex}\n";
  List.iter
    (fun (surname, first_name, numbers) ->
      let comma = if first_name <> "" then ", " else "" in
      output_string oc
        (Format.sprintf "%s%s%s%s :%s\n"
           (* account for "\item Surname ..." *)
           (let head =
              if String.length (Name.lower surname) > 6 then
                (Name.lower surname).[6]
              else ' '
            in
            if head <> !prev_head then (
              prev_head := head;
              "\n\\indexspace\n\n")
            else "")
           surname comma first_name numbers))
    new_entries;
  output_string oc "\n\\end{theindex}\n";
  close_in ic;
  close_out oc;
  let move_index_back = Printf.sprintf "mv %s %s" out_file in_file in
  if !verbose then Printf.eprintf "Commd: %s\n" move_index_back;
  let _ = Sys.command move_index_back in
  ()

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
