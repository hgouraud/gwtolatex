(* Copyright (c) 2013 H.Gouraud *)

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

type key = { pk_first_name : string; pk_surname : string; pk_occ : int }
(** Key to refer a person's definition *)

let print_key key =
  Printf.eprintf "%s.%d+%s\n" key.pk_first_name key.pk_occ key.pk_surname

let print_dict1_entry image_id dict1 =
  let anx_page, desc, fname, key_l, _key_l_2, _image_occ =
    Hashtbl.find dict1 image_id
  in
  Printf.eprintf "%d;%d;%s;%s\n" image_id anx_page desc fname;
  List.iter
    (fun (key : key) ->
      Printf.eprintf "\\index{%s, %s}%s\n" key.pk_surname key.pk_first_name
        (if key.pk_occ = 0 then "" else string_of_int key.pk_occ))
    key_l

let print_dict1 dict1 =
  Hashtbl.iter (fun image_id _ -> print_dict1_entry image_id dict1) dict1

let print_dict2_entry (key : key) dict1 dict2 =
  let img_list = Hashtbl.find dict2 key in
  print_key key;
  List.iter
    (fun image_id ->
      let anx_page, desc, fname, _key_l, _key_l_2, _image_occ =
        Hashtbl.find dict1 image_id
      in
      Printf.eprintf "%d;%d;%s;%s\n" image_id anx_page desc fname)
    img_list

let print_dict2 dict1 dict2 =
  Hashtbl.iter (fun key _ -> print_dict2_entry key dict1 dict2) dict2

let undo_particle sn =
  let i = try String.index sn '(' with Not_found -> -1 in
  let j = try String.index sn ')' with Not_found -> -1 in
  let particle =
    if i <> -1 && j <> -1 then String.sub sn (i + 1) (j - i - 1) else ""
  in
  let surname = if i <> -1 && j <> -1 then String.sub sn 0 (i - 1) else sn in
  let apostr =
    (j > 0 && sn.[j] = '\'')
    || j > 3
       && Char.code sn.[j - 3] = 0xE0
       && Char.code sn.[j - 2] = 0x80
       && (Char.code sn.[j - 1] = 0x93 || Char.code sn.[j - 1] = 0x94)
  in
  if particle = "" then sn
  else Format.sprintf "%s%s%s" particle (if apostr then "" else " ") surname

(*
Read Livres/family-input/who_is_where.tex
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

(* # image_id;n;texte descriptif;nom-de-fichier.jpg *)
(* # voir ... *)
(* \index{X, Y}/z  ;z indique N° d'occurrence, "z" si ?? *)

(* construit dict1 :
   Hashtbl.add !dict1 image_id (anx_page, desc, fname, key_l, [], 0) *)
let process dict1 ic line =
  try
    let parts = String.split_on_char ';' line in
    if line = "" then ()
    else if List.length parts <> 4 then
      Printf.eprintf "Bad image definition %s\n" line
    else
      let image_id = int_of_string (List.nth parts 0) in
      let anx_page = int_of_string (List.nth parts 1) in
      let desc = List.nth parts 2 in
      let fname = List.nth parts 3 in
      if Hashtbl.mem !dict1 image_id then
        Printf.eprintf "Duplicate image definition %d, %s\n" image_id desc
      else
        let line = Sutil.input_real_line ic in
        let key_l =
          let rec loop line key_l =
            if line = "" then key_l
            else if Sutil.start_with "\\index" 0 line then
              let i = try String.index line '{' with Not_found -> -1 in
              let j = try String.index line '}' with Not_found -> -1 in
              if i <> -1 && j <> -1 then
                let str = String.sub line (i + 1) (j - i - 1) in
                let ocn =
                  if String.length line > j + 2 then
                    String.sub line (j + 2) (String.length line - j - 2)
                  else "0"
                in
                let ocn = if ocn = "z" then "-1" else ocn in
                let parts = String.split_on_char ',' str in
                let sn =
                  if List.length parts > 0 then List.nth parts 0 else ""
                in
                let fn =
                  if List.length parts > 1 then List.nth parts 1 else ""
                in
                let fn =
                  if String.length fn > 1 && fn.[0] = ' ' then
                    String.sub fn 1 (String.length fn - 1)
                  else fn
                in
                let i = try String.index fn '(' with Not_found -> -1 in
                let j = try String.index fn ')' with Not_found -> -1 in
                let fn =
                  if
                    i <> -1 && j <> -1
                    && String.length fn > i + 2
                    && fn.[i + 1] = 'e'
                    && fn.[i + 2] = 'p'
                  then String.sub fn 0 (i - 1)
                  else fn
                in
                let sn = undo_particle sn in
                let sn = Sutil.replace ' ' '_' sn in
                let fn = Sutil.replace ' ' '_' fn in
                let (key : key) =
                  {
                    pk_first_name = fn;
                    pk_surname = sn;
                    pk_occ = int_of_string ocn;
                  }
                in
                if sn = "" && fn = "" then loop (input_line ic) key_l
                else loop (Sutil.input_real_line ic) (key :: key_l)
              else key_l
            else key_l
          in
          loop line []
        in
        Hashtbl.add !dict1 image_id (anx_page, desc, fname, key_l, [], [])
  with
  | Failure _ -> Printf.eprintf "Bad image definition %s\n" line
  | End_of_file -> ()

(** dict1 image_id, (annex_page, description, file_name, person_list, occ)
    dict2 person_key, images_id list
    dict3 file_name, image_id *)
let create_images_dicts img_file fam_file =
  Printf.eprintf "Create images dicts\n";
  let dict1 = ref (Hashtbl.create 100) in
  let dict2 = Hashtbl.create 100 in
  let dict3 = Hashtbl.create 100 in

  let ic = open_in fam_file in
  let rec loop acc line =
    match line with
    | Some line ->
        if Sutil.start_with "<a href" 0 line then
          if Sutil.contains line "&p" && Sutil.contains line "&n" then
            let parts1 = String.split_on_char '&' line in
            let parts2 =
              List.fold_left
                (fun acc p ->
                  let kv = String.split_on_char '=' p in
                  if List.length kv = 2 then
                    (List.nth kv 0, List.nth kv 1) :: acc
                  else acc)
                [] parts1
            in
            let fn = List.assoc "p" parts2 in
            let sn = List.assoc "n" parts2 in
            let oc = try List.assoc "oc" parts2 with Not_found -> "0" in
            let (key : key) =
              { pk_first_name = fn; pk_surname = sn; pk_occ = int_of_string oc }
            in
            loop (key :: acc) (Sutil.read_line ic)
          else loop acc (Sutil.read_line ic)
        else loop acc (Sutil.read_line ic)
    | None ->
        close_in ic;
        acc
  in
  (*liste des personnes ayant une page perso *)
  let list4 = loop [] (Sutil.read_line ic) in

  let ic = open_in img_file in
  let rec loop line =
    match line with
    | Some line ->
        process dict1 ic line;
        loop (Sutil.read_line ic)
    | None -> close_in ic
  in
  loop (Sutil.read_line ic);

  (* filtre key_l pour ne garder que ceux presents -> key_l_2 *)
  Hashtbl.filter_map_inplace
    (fun _image_id (anx_page, desc, fname, key_l, _key_l_2, occ) ->
      let key_l_2 =
        List.fold_left
          (fun acc key -> if List.mem key list4 then key :: acc else acc)
          [] key_l
      in
      Some (anx_page, desc, fname, key_l, key_l_2, occ))
    !dict1;

  Printf.eprintf "Dict1 (%d)\n" (Hashtbl.length !dict1);

  (* dict1 has been built, now we build the inverse indexes *)
  (* liste des images sur lesquelles apparait une personne *)
  (* en passant dict3 : fname -> image_id *)
  (* dict2 -> key image_id list *)
  Hashtbl.iter
    (fun image_id (_anx_page, _desc, fname, key_l, _key_l_2, _occ) ->
      Hashtbl.add dict3 fname image_id;
      List.iter
        (fun key ->
          match Hashtbl.find_opt dict2 key with
          | Some x -> Hashtbl.replace dict2 key (image_id :: x)
          | None -> Hashtbl.add dict2 key [ image_id ])
        key_l)
    !dict1;

  Printf.eprintf "Dict2 (%d)\n" (Hashtbl.length dict2);

  (* create list_assoc [fname, image_id] *)
  let img_fname_l =
    Hashtbl.fold (fun fname image_id acc -> (fname, image_id) :: acc) dict3 []
  in
  let img_fname_l =
    List.sort_uniq (fun (_fname1, id1) (_fname2, id2) -> id1 - id2) img_fname_l
  in
  (!dict1, dict2, img_fname_l)
