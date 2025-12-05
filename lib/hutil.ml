(* html utilities *)
(* v1  Henri, 2023/10/16 *)

module Gwdb = Geneweb_db.Driver

let dummy_sn = [ "."; "X" ]

let dummy_fn =
  [
    "famille";
    "Famille";
    "Pêcheurs";
    "Carriers";
    "SCI";
    "Phare";
    "Maisons";
    "Ruines";
    "Grande île, côté Public";
    "Grande île, côté SCI";
    "Ile d'Aneret";
    "Bateaux";
    "Bisquines";
    "Canots mixtes";
    "Canots à moteur a";
    "Canots à moteur b";
    "Canots en plastique";
    "Canots Chausiais";
    "Canots à Voile";
    "Caravelles et Dériveurs";
    "Doris";
    "Plaisance";
    "Vedettes";
    "Vedettes passagers";
    "Vedettes passagers anciennes";
    "Divers";
    "Anciens Propriétaires";
    "Divers";
    "Iles";
  ]

let get_real_person base i p n oc _content =
  (* TODO check <> content *)
  let p = Name.lower p in
  let n = Name.lower n in
  let ip =
    match
      Gwdb.person_of_key base p n (try int_of_string oc with Failure _ -> 0)
    with
    | Some ip -> ip
    | None -> ( try Gwdb.Iper.of_string i with Failure _ -> Gwdb.Iper.dummy)
  in
  let get_spouse base iper ifam =
    let f = Gwdb.foi base ifam in
    if iper = Gwdb.get_father f then Gwdb.poi base (Gwdb.get_mother f)
    else Gwdb.poi base (Gwdb.get_father f)
  in
  let person = Gwdb.poi base ip in
  let fn = Gwdb.sou base (Gwdb.get_first_name person) in
  let sn = Gwdb.sou base (Gwdb.get_surname person) in
  let sex = Gwdb.get_sex person in
  let ocn = try Gwdb.get_occ person with Failure _ -> 0 in
  (* TODO verify uppercase! (le Fort), (Le Fort) *)
  let fams = Gwdb.get_family person in
  let sn1 = Sutil.particles sn in
  let sp =
    if sex = Female && Array.length fams > 0 then
      let ifam = fams.(Array.length fams - 1) in
      let sp = get_spouse base ip ifam in
      Some (Gwdb.sou base (Gwdb.get_surname sp))
    else None
  in
  let sp1 =
    match sp with Some sp -> Format.sprintf " (ep %s)" sp | None -> ""
  in
  let sp2 =
    if sex = Female then
      match sp with
      | Some sp -> Format.sprintf "\\index{%s, %s (née %s)}" sp fn sn
      | None -> ""
    else ""
  in
  let ocnn = if ocn = 0 then "" else Format.sprintf " (%d)" ocn in
  let aliases = Gwdb.get_aliases person in
  let ind1 =
    if List.mem sn dummy_sn && fn <> "" then Format.sprintf "\\index{%s}" fn
    else if List.mem fn dummy_fn && sn1 <> "" then
      Format.sprintf "\\index{%s}" sn1
    else if sn1 <> "" && fn <> "" then
      Format.sprintf "\\index{%s, %s%s%s}%s" sn1 fn ocnn sp1 sp2
    else ""
  in
  let aliases =
    List.map
      (fun a ->
        let a = Gwdb.sou base a in
        if
          List.mem fn dummy_fn || List.mem sn dummy_sn || a = ""
          || (sn1 = "" && fn = "")
        then ""
        else Format.sprintf "\\index{%s, voir %s, %s%s}" a sn1 fn ocnn)
      aliases
  in
  let ind2 = String.concat "" aliases in
  let index_s = if sn <> "?" && fn <> "?" then ind1 ^ ind2 else "" in
  (fn, sn, ocn, sp, index_s)

let test_attr attributes attr value =
  List.exists (fun ((_, k), v) -> k = attr && v = value) attributes

let get_attr attributes attr =
  List.fold_left
    (fun c ((_, k), v) -> if k = attr then v ^ c else c)
    "" attributes

let show_attr attributes =
  List.fold_left (fun c ((_, k), v) -> k ^ "=" ^ v ^ ", " ^ c) "" attributes

(* <a href="base?m=IM&p=first_name&n=surname&occ=noc&k=first_name.noc.surname" *)
(* <a href="base?m=IM;s=test/filaname.jpg"> *)
(* <a href="base_token?m=IM;s=test/filaname.jpg"> *)
(* Chausey\_{}qnnvsntxq?templ=tex\&{}m=IM *)
(* Href: "%sm=IM;s=famille-charnace.jpg" border="0" usemap="#Famille-Charnace" *)

let split_href href =
  let href = List.nth (String.split_on_char ' ' href) 0 in
  let href = Sutil.convert_html href |> Sutil.decode in
  let parts = String.split_on_char '?' href in
  let href =
    Sutil.replace ';' '&'
      (List.nth parts (if List.length parts = 2 then 1 else 0))
    |> Sutil.replace '"' ' ' |> String.trim
  in
  let evars = String.split_on_char '&' href in
  let evars =
    List.map
      (fun kv ->
        let tmp = String.split_on_char '=' kv in
        (List.nth tmp 0, if List.length tmp > 1 then List.nth tmp 1 else ""))
      evars
  in
  let evars =
    (* & have been escaped as \&{} !! *)
    List.map
      (fun (k, v) ->
        ( (if String.length k > 2 && k.[0] = '{' && k.[1] = '}' then
             String.sub k 2 (String.length k - 2)
           else k),
          if String.length v > 0 && v.[String.length v - 1] = '\\' then
            String.sub v 0 (String.length v - 1)
          else v ))
      evars
  in
  if List.mem_assoc "b" evars then evars
  else
    let server = List.nth parts 0 in
    let b =
      let j = try String.index server '_' with Not_found -> -1 in
      if j <> -1 then String.sub server 0 j else server
    in
    ("b", b) :: evars
(* TODO treat CGI case *)

let get_href_attr attr attrl =
  if List.mem_assoc attr attrl then List.assoc attr attrl else ""

(* <a href="http://127.0.0.1:2317/base?p=sylvie&n=sautin&templ=tex">Henri</a> *)
let get_href line =
  let line = Sutil.replace ';' '&' line in
  let i = try String.index line '"' with Not_found -> -1 in
  let j = try String.index_from line (i + 1) '"' with Not_found -> -1 in
  if i <> -1 && j <> -1 then String.sub line i (j - i) else ""
