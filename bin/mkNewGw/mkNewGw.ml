(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

(*
scan family.gw file producing family-new.gw
insert information in notes (create notes if needed)
indicating the images where the person is visible.
Gets the data from the dictionaries created by mkImgDict
*)

(* Option qui force a créer les clés des individus. De fait, *)
(* si la clé est incomplète, on l'enregistre tout de même.  *)
let _create_all_keys = ref false

(** Parsing status of .gw block  *)
type 'a read_family =
  | F_notes of MkImgDict.key * string  (** a notes block inside .gw file *)
  | F_other of string * string list  (** other content *)
  | F_none  (** end of the file *)
  | F_fail of string  (** exception while reading *)

(* execution context *)
let base = ref ""
let passwd = ref ""
let gw_dir = ref (try Sys.getenv "GW_BIN" with Not_found -> "./")
let family = ref ""
let out_file = ref ""
let debug = ref 0
let dev = ref false
let verbose = ref false
let no_fail = ref false

(* Assumes we are running in bases folder GeneWeb security constraint *)
let livres = ref ""
let bases = ref ""
let test = ref false
let test_nb = ref 0

(** Parses int that starts at the position [i] inside [x].
    Raises [Not_found] if integer isn't found. *)
let make_int x =
  let rec loop found n i =
    if i = String.length x then if found then n else raise Not_found
    else
      match x.[i] with
      | '0' .. '9' as c ->
          loop true ((10 * n) + Char.code c - Char.code '0') (succ i)
      | _ -> raise Not_found
  in
  loop false 0

(** Removes spaces at the begining an at the end of string. *)
let cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len - 1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)

(** from Geneweb.Mutil *)
let strip_all_trailing_spaces s =
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

(** [copy_decode s i1 i2] decode the word delimited by [i1] and [i2] inside [s]
    by remplacing "\\" -> '\' and '_' -> ' ' *)
let copy_decode s i1 i2 =
  let len =
    let rec loop len i =
      if i >= i2 then len
      else if i = i2 - 1 then len + 1
      else if s.[i] = '\\' then loop (len + 1) (i + 2)
      else loop (len + 1) (i + 1)
    in
    loop 0 i1
  in
  let rec loop_copy t i j =
    if i >= i2 then Bytes.unsafe_to_string t
    else if i = i2 - 1 && s.[i] <> '_' then (
      Bytes.set t j s.[i];
      Bytes.unsafe_to_string t)
    else
      let c, i =
        match s.[i] with
        | '_' -> ('_', i)
        | '\\' -> (s.[i + 1], i + 1)
        | x -> (x, i)
      in
      Bytes.set t j c;
      loop_copy t (succ i) (succ j)
  in
  loop_copy (Bytes.create len) i1 0

(** Return list of words inside the [str] *)
let fields str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
      | ' ' | '\t' ->
          if beg = i then loop (succ beg) (succ i)
          else copy_decode str beg i :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [ copy_decode str beg i ]
  in
  loop 0 0

(** Line counter while reading .gw file *)
let line_cnt = ref 0

(** Read line from input channel. *)
let input_a_line ic =
  let line = input_line ic in
  incr line_cnt;
  if String.length line > 0 && line.[String.length line - 1] = '\r' then
    String.sub line 0 (String.length line - 1)
  else line

(** Read a line. If line is empty or only contains a comment (#), then read next line  *)
let input_real_line ic = input_line ic

let read_line ic =
  try
    let str = input_real_line ic in
    Some (str, fields str)
  with End_of_file -> None

let read_notes ic =
  let notes =
    try
      let rec loop = function
        | "end notes" -> ""
        | l -> l ^ "\n" ^ loop (input_a_line ic)
      in
      loop (input_a_line ic)
    with End_of_file -> failwith "end of file"
  in
  strip_all_trailing_spaces notes

let get_name str l =
  match l with surname :: l -> (surname, l) | _ -> failwith str

let get_fst_name str l =
  match l with
  | x :: l' -> (
      match x.[0] with
      (*'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' *)
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '\xE0' .. '\xFF'
      | '\xC0' .. '\xDD'
      | '['
      | '0' .. '9'
      | '?' | ' ' ->
          let x = cut_space x in
          let x, occ =
            match String.rindex_opt x '.' with
            | Some i -> (
                try (String.sub x 0 i, make_int x (succ i))
                with Not_found -> (x, 0))
            | None -> (x, 0)
          in
          (x, occ, l')
      | _ -> failwith str)
  | _ -> failwith str

let read_family ic = function
  (* Notes block *)
  | Some (str, "notes" :: l) -> (
      let surname, l = get_name str l in
      let first_name, occ, l = get_fst_name str l in
      if l <> [] then failwith "str"
      else
        match read_line ic with
        | Some (_, [ "beg" ]) ->
            let notes = read_notes ic in
            let (key : MkImgDict.key) =
              { pk_first_name = first_name; pk_surname = surname; pk_occ = occ }
            in
            F_notes (key, notes)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file")
  | Some (str, line) -> F_other (str, line)
  (* End of the file *)
  | None -> F_none

(** Read and return a block of .gw file. If [!no_fail] is disabled raises
    [Failure] exception. *)
let read_family_1 ic line =
  if !no_fail then try read_family ic line with Failure str -> F_fail str
  else read_family ic line

let output_string_nl oc str =
  output_string oc str;
  output_string oc "\n"

(** list of persons having notes *)
let has_notes = ref []

let notes_header oc (key : MkImgDict.key) =
  let sn = Sutil.replace ' ' '_' key.pk_surname in
  let fn = Sutil.replace ' ' '_' key.pk_first_name in
  if sn = "Guerin" || fn = "Guérin" then
    Printf.eprintf "notes for %s %s\n" fn sn;
  output_string oc
    (Format.sprintf "notes %s %s%sbeg\n" sn fn
       (if key.pk_occ <> 0 then Format.sprintf ".%d\n" key.pk_occ else "\n"))

(** prints the list of images on which person appears
    images x.y.z.w on page ppp, or image in annex page n
    where x.y.z.w should be a section/img number
    pendant la création du fichier .tex, on calculera
    \newcommand{ref_image_id}{ch.sec.subs.nbr} et
    \label{label_img_image_id}
    à partir du nom de fichier
*)

let print_img_list oc images_l dict1 =
  output_string oc
    ((Format.sprintf
        {|
<span style="display:none" mode="tex">
\medskip
\textit[Présent(e) aussi sur %s |})
       (if List.length images_l = 1 then "la photo" else "les photos"));
  let img_l =
    List.fold_left
      (fun acc image_id ->
        let anx_page, desc, _fname, _index_l = Hashtbl.find dict1 image_id in
        Format.sprintf {|"%s" (%s)|} desc
          (if anx_page <> "0" then Format.sprintf "page %s en annexe" anx_page
          else
            Format.sprintf "\\ref[img_ref_%s] page \\pageref[img_ref_%s]"
              image_id image_id)
        :: acc)
      [] images_l
  in
  output_string oc (String.concat ",\n" img_l);
  output_string oc "]</span>\n"

(** add image information at end of notes *)
let print_notes oc key notes dict1 dict2 cnt =
  notes_header oc key;
  output_string_nl oc notes;
  let key_str =
    Format.sprintf "%s.%d+%s" key.pk_first_name key.pk_occ key.pk_surname
  in
  if Hashtbl.mem dict2 key then (
    incr cnt;
    print_img_list oc (Hashtbl.find dict2 key) dict1;
    has_notes := key_str :: !has_notes);
  output_string oc "end notes\n"

(** create notes for persons which did not have then.
    insert image information *)
let create_new_notes oc has_notes dict1 dict2 =
  let cnt1 = ref 0 in
  let cnt2 = ref 0 in
  Hashtbl.iter
    (fun (key : MkImgDict.key) _val ->
      let key_str =
        Format.sprintf "%s.%d+%s" key.pk_first_name key.pk_occ key.pk_surname
      in
      if (not (List.mem key_str has_notes)) && key.pk_occ <> -1 then (
        incr cnt1;
        notes_header oc key;
        print_img_list oc (Hashtbl.find dict2 key) dict1;
        output_string oc "end notes\n\n")
      else if key.pk_occ = -1 then incr cnt2)
    dict2;
  if !verbose then
    Printf.eprintf "%d new notes created (%d ignored)\n" !cnt1 !cnt2

let comp_families ic oc in_file out_file dict1 dict2 =
  has_notes := [];
  line_cnt := 0;
  let cnt = ref 0 in
  (try
     let rec loop line =
       match read_family_1 ic line with
       | F_notes (key, notes) ->
           print_notes oc key notes dict1 dict2 cnt;
           loop (read_line ic)
       | F_other (str, _) ->
           output_string_nl oc str;
           loop (read_line ic)
       | F_none -> ()
       | F_fail str ->
           Printf.printf "File \"%s\", line %d:\n" in_file !line_cnt;
           Printf.printf "Error: %s\n" str;
           flush stdout;
           loop (read_line ic)
     in
     loop (read_line ic);
     if !verbose then Printf.eprintf "%d notes updated\n" !cnt;
     close_in ic;
     create_new_notes oc !has_notes dict1 dict2
   with e ->
     close_out oc;
     Mutil.rm out_file;
     raise e);
  close_out oc

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
    "This is \027[32mmakeNewGw\027[0m version %s for base %s (%d)\n"
    Sutil.version !base !debug;
  flush stderr;

  let in_file = Filename.concat "." (!base ^ ".gw") in
  let out_file = Filename.concat "." (!base ^ "-new.gw") in

  let ic = open_in in_file in
  let oc = open_out out_file in

  if !debug = -1 then Sys.enable_runtime_warnings false;

  let img_file =
    String.concat Filename.dir_sep
      [ !livres; !family ^ "-inputs"; "who_is_where.txt" ]
  in

  (* build images dictionnaries *)
  let dict1, dict2, _dict3 = MkImgDict.create_images_dicts img_file in
  (* Printf.eprintf "Done creating images dictionaries (%d images, %d persons)\n"
      (Hashtbl.length dict1) (Hashtbl.length dict2) ;*)
  comp_families ic oc in_file out_file dict1 dict2;
  Printf.eprintf "\n";
  close_in ic;
  close_out oc

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
