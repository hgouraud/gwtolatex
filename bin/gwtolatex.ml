(* Copyright (c) 2013 H.Gouraud *)
type name = string * string

(* TODO suppress (pages liées) and (modifier) in m=NOTES *)
(* TODO suppress "base chausey ..." *)
type my_tree =
  | Text of string
  | Element of string * (name * string) list * my_tree list

type im_type = Portrait | Imagek | Images

type _image = {
  im_type : im_type;
  filename : string;
  where : int * int * int * int; (* ch, sec, ssec, sssec*)
  image_nbr : int;
}

let im_width_default = "5.1cm"
let im_width = ref im_width_default

(*
(* width, span, (F O Hr Hc Hl Vc c), item, text, index *)
type t_cell = {
  width : float;
  span : int;
  typ : string;
  item : string;
  txt : string;
  index : int;
}

type t_line = t_cell list

type t_table = {
  row : int;
  col : int;
  body: t_line list;
}
*)

let new_tree = ref [ [] ]
let new_row = ref []
let c_width = ref 0
let c_span = ref 1
let c_typ = ref ""
let c_txt = ref ""
let c_item = ref ""

(* execution context *)
let base = ref "chausey"
let family = ref ""
let out_file = ref ""
let debug = ref false
let index = ref 0
let verbose = ref false

(* TODO manage Livres and bases references. ? env variables? *)
let livres =
  ref
    (try Sys.getenv "GWTL_LIVRES"
     with Not_found -> "/Users/Henri/Genea/Livres")

let bases =
  ref
    (try Sys.getenv "GWTL_BASES"
     with Not_found -> "/Users/Henri/Genea/GeneWeb-Bases")

let test = ref false
let follow = ref false
let test_nb = ref 0
let level = ref 0
let version = "1.0"

(* current values *)
let current_level = ref 0
let images_in_page = ref []
let chapter = ref 0
let section = ref 0
let subsection = ref 0
let subsubsection = ref 0
let image_nbr = ref 0
let image_label = ref 3
let collect_images = ref false
let wide = ref false
let section_on_a_tag = ref false
let highlights = ref []
let _width = ref 7
let ch_nb_in_fig_nb = ref true
let sub = ref false
let _trees = ref false
let ep = ref false
let arbres = ref false
let _sideways = ref false
(*let first_tr = ref true
let first_td = ref true
let td_nbr = ref 0*)

let open_base basename =
  match basename with
  | "" ->
      Printf.eprintf "No basename supplied\n";
      exit 1
  | bfile -> (
      let bfile = bfile ^ ".gwb" in
      let base = try Some (Gwdb.open_base bfile) with _ -> None in
      match base with
      | None ->
          Printf.eprintf "Cannot open base %s\n" bfile;
          exit 1
      | Some base -> base)

(* TODO find a way to open base remotely *)
let my_base = ref (open_base (Filename.concat "." !base))

let _strip_nl s =
  let b = Buffer.create 10 in
  String.iter
    (fun c -> if c = '\n' then Buffer.add_char b ' ' else Buffer.add_char b c)
    s;
  Buffer.contents b

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

let contains str sub =
  let strlen = String.length str in
  let sublen = String.length sub in
  let rec aux i1 i2 =
    if i1 = sublen then true
    else if i2 = strlen then false
    else if String.unsafe_get str i2 = String.unsafe_get sub i1 then
      aux (i1 + 1) (i2 + 1)
    else false
  in
  let rec loop i =
    if i + sublen <= strlen then aux 0 i || loop (i + 1) else false
  in
  loop 0

let chop_body n body = String.sub body 0 (min n (String.length body))

(* returns content between matching tags, and following body *)
let _find_matching_tag name body =
  if !level = 1 then
    Printf.eprintf "Find matching tag: name: %s body: (%d) %s\n" name
      (String.length body) (chop_body 60 body);
  let rec match_tag i body =
    if i >= String.length body - 1 then ("", "")
    else
      let j =
        try String.index_from body i '<'
        with Not_found | Invalid_argument _ -> -1
      in
      if j = -1 then (
        Printf.eprintf "Cannot find matching tag: %s, %s\n" name
          (chop_body 50 body);
        ("", ""))
      else if String.length body > j + 1 && body.[j + 1] = '/' then (
        let found =
          let rec loop k =
            if k = String.length name then true
            else if body.[j + 2 + k] = name.[k] then loop (k + 1)
            else false
          in
          loop 0
        in
        if !level = 1 then
          Printf.eprintf "Found: %s, (%d)\n"
            (if found then "yes" else "no")
            (String.length body);
        if found then (
          (* <tag>content</tag>, body *)
          let content = String.sub body 0 j in
          if !level = 1 then
            Printf.eprintf "Body3: name: %s, %d (%s) (%s)\n" name j
              (chop_body 30 body) content;
          let body =
            String.sub body
              (j + 3 + String.length name)
              (String.length body - (j + 3 + String.length name))
          in
          if !level = 1 then Printf.eprintf "Body4: (%s)\n" (chop_body 30 body);
          let body =
            if body <> "" && body.[0] = '\n' then
              String.sub body 1 (String.length body - 1)
            else body
          in
          if !level = 1 then Printf.eprintf "Body5: (%s)\n" (chop_body 30 body);
          (content, body))
        else match_tag (j + 1) body)
      else match_tag (j + 1) body
  in
  match_tag 0 body


(* scan <a href="...">xxx</a> * to extract xxx *)
let get_a_content line =
  let i = try String.index_from line 0 '>' with Not_found -> -1 in
  let j = try String.index_from line i '<' with Not_found -> -1 in
  if i <> -1 && j <> -1 then String.sub line (i + 1) (j - i - 1) else ""

let rec dump children l =
  let tab l =
    let rec loop acc i = if i = l then acc else loop (acc ^ "..") (i + 1) in
    loop ".." 0
  in
  Printf.eprintf "%sChildren: %d\n" (tab l) (List.length children);
  List.iter
    (fun elt ->
      match elt with
      | Text t -> Printf.eprintf "%sTxt: (%s)\n" (tab l) t
      | Element (n, _a, c) ->
          Printf.eprintf "%sElt: %s\n" (tab l) n;
          dump c (l + 1))
    children;
  Printf.eprintf "%sEnd children:\n" (tab l)

let dump_tag elt =
  match elt with
  | Element (name, attributes, children) ->
      Printf.eprintf "<begin %s>\n" name;
      List.iter
        (fun ((_a, b), c) -> Printf.eprintf "  attr: %s=%s\n" b c)
        attributes;
      dump children 0;
      Printf.eprintf "<end %s>\n" name
  | Text s -> Printf.eprintf "Text elt: (%s)\n" s

(* in str, replace car x by car y *)
let replace x y str =
  let b = Buffer.create 40 in
  String.iter
    (fun c -> if c = x then Buffer.add_char b y else Buffer.add_char b c)
    str;
  Buffer.contents b

let unaccent_utf_8 lower s i =
  let fns =
    if lower then fun n s -> (String.lowercase_ascii s, n) else fun n s -> (s, n)
  in
  let fnc =
    if lower then fun n c -> (String.make 1 @@ Char.lowercase_ascii c, n)
    else fun n c -> (String.make 1 c, n)
  in
  let s, n =
    Unidecode.decode fns fnc
      (fun n -> (String.sub s i (n - i), n))
      s i (String.length s)
  in
  if lower then (String.lowercase_ascii s, n) else (s, n)

let lower s =
  let rec copy special i len =
    if i = String.length s then Buff.get len
    else if Char.code s.[i] < 0x80 then
      match s.[i] with
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.') as c ->
          let len = if special then Buff.store len ' ' else len in
          let c = Char.lowercase_ascii c in
          copy false (i + 1) (Buff.store len c)
      | _ -> copy (len <> 0) (i + 1) len
    else
      let len = if special then Buff.store len ' ' else len in
      let t, j = unaccent_utf_8 true s i in
      copy false j (Buff.mstore len t)
  in
  copy false 0 0

(* TODO find TaTeX equivalent string *)
(*
    & % $ # _ { } ~ ^ \

Outside \verb, the first seven of them can be typeset 
by prepending a backslash; for the other three,
use the macros \textasciitilde, \textasciicircum, and \textbackslash.
*)

let escape str =
  let special =
    [
      ('&', "\\&{}");
      ('%', "\\%{}");
      ('#', "\\#{}");
      ('~', "\\~{}");
      ('^', "\\^{}");
      ('_', "\\_{}");
      (* dont escape $, {, } and \, we need them in the content *)
    ]
  in
  let b = Buffer.create 100 in
  String.iter
    (fun c ->
      try
        let s = List.assoc c special in
        Buffer.add_string b s
      with Not_found -> Buffer.add_char b c)
    str;
  Buffer.contents b

let get_att_list attributes =
  List.fold_left (fun acc ((_, k), v) -> (k, v) :: acc) [] attributes

let split_href href =
  let parts = String.split_on_char '?' href in
  let href =
    replace ';' '&' (List.nth parts (if List.length parts = 2 then 1 else 0))
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
    (* & have been escaped !! *)
    List.map
      (fun (k, v) ->
        ( k,
          if String.length v > 0 && v.[String.length v - 1] = '\\' then
            String.sub v 0 (String.length v - 1)
          else v ))
      evars
  in
  if !level = 1 then
    List.iter (fun (k, v) -> Printf.eprintf "%s=(%s)\n" k v) evars;
  let b =
    try List.assoc "b" evars
    with Not_found ->
      let server = List.nth parts 0 in
      let j =
        try String.rindex_from server (String.length server - 1) '/'
        with Not_found -> -1
      in
      if j <> -1 then String.sub server j (String.length server - j - 1) else ""
  in
  let m = try List.assoc "m" evars with Not_found -> "" in
  let p = try List.assoc "p" evars with Not_found -> "" in
  let n = try List.assoc "n" evars with Not_found -> "" in
  let oc = try List.assoc "oc" evars with Not_found -> "" in
  let i = try List.assoc "i" evars with Not_found -> "" in
  let k = try List.assoc "k" evars with Not_found -> "" in
  let s = try List.assoc "s" evars with Not_found -> "" in
  let v = try List.assoc "v" evars with Not_found -> "" in
  (b, m, p, n, oc, i, k, s, v)

let print_image (im_type, name, (ch, sec, ssec, sssec), nb) =
  let _trace =
    Format.sprintf "Type: %s, name: %s, (%d, %d, %d, %d), nb: %d"
      (match im_type with
      | Portrait -> "Portrait"
      | Images -> "Images"
      | Imagek -> "Imagek")
      name ch sec ssec sssec nb
  in
  match im_type with
  | Portrait | Imagek ->
      Format.sprintf "\n\\includegraphics[width=5cm]{%s/%s.%s}\n"
        "/Users/Henri/Genea/GeneWeb-Bases/images/chausey/Side"
        (lower name |> replace '.' '-' |> replace ' ' '_')
        "jpg"
  | Images ->
      let ext = Filename.extension name in
      let ext = String.sub ext 1 (String.length ext - 1) in
      let name = Filename.remove_extension name in
      Format.sprintf "\n\\includegraphics[width=%s]{%s/%s.%s}\n"
        !im_width "/Users/Henri/Genea/GeneWeb-Bases/src/chausey/images" name ext

(* ignore tag but read children *)
let dummy_tags_0 = [ "body"; "html"; "center" ]

(* ignore tag, skip to end *)
let dummy_tags_1 = [ "!--"; "bdo"; "samp"; "table"; ]

let dummy_tags_2 =
  [
    "col";
    "!DOCTYPE";
    "!doc";
    "imgsrc";
    "fontcolor";
    "input";
    "link";
    "meta";
    "nav";
    "option";
  ]

let dummy_tags_3 =
  [
    "button";
    "head";
    "form";
    "select";
    "colgroup";
    "font";
    "script";
    "title";
    "style";
  ]

let simple_tag_1 t str =
  let tags =
    [ ("i", "textit");  ("u", "underline"); ("b", "bf");
      ("em", "it"); ("strong", "bf"); ("big", "large");
      ("small", "small"); ("tiny", "tiny"); ("tt", "tt")]
  in
  let cmd =
    try List.assoc t tags
    with Not_found ->
      Printf.eprintf "funny tag 1 %s\n" t;
      "underline"
  in
  if str <> "" then Format.sprintf "{\\%s %s}" cmd str else ""

let _simple_tag_2 t str =
  let tags = [ ("b", "bf"); ("small", "scriptsize"); ("tiny", "tiny"); ("tt", "tt"); ] in
  let cmd =
    try List.assoc t tags
    with Not_found ->
      Printf.eprintf "funny tag 2 %s\n" t;
      "underline"
  in
  if str <> "" then Format.sprintf "{\\%s %s}" cmd str else ""

(* convert %xx utf-8 notation *)
let decode s =
  let hexa_val conf =
    match conf with
    | '0' .. '9' -> Char.code conf - Char.code '0'
    | 'a' .. 'f' -> Char.code conf - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code conf - Char.code 'A' + 10
    | _ -> 0
  in
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with '%' -> true | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        | '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        | '%' when i + 2 < String.length s ->
            let v = (hexa_val s.[i + 1] * 16) + hexa_val s.[i + 2] in
            Bytes.set s1 i1 (Char.chr v);
            i + 3
        | x ->
            Bytes.set s1 i1 x;
            succ i
      in
      copy_decode_in s1 i (succ i1)
    else Bytes.unsafe_to_string s1
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    let s = copy_decode_in s1 0 0 in
    s
  else s

(* process commands to build a full page page *)
(* used for tree displays and large images *)
(* various commands govern the layout *)
(* as well as captions, labels and fig numbers *)

(* example:

   <y Split a html colon driven table across two pages (two passes)>
   <y split paheheight_left paheheight_right Nbr_of_cols_in_left_page nbr_of_cols_in_right_page LR=1 Left first>
   <y good numbers are 17 (after chapter heading), 23 (after section heading), 24 (std page)>
   <y table is 59 wide>
   <y trial and errors !!>

   <begin page>
   <sideways="1"
   <split="23 24 32 28 RthenL">
   <title="Arbre descendant d'Albert Marie (Toumagi)">
   <index="Marie, Albert (Toumagi)">
   <href="http://127.0.0.1:2317/Chausey?m=D;p=albert;n=marie;v=2;image=off;t=T;dag=on;templ=tex;w=hg:1045">
   <end>

   <begin page>
   <sideways="1">
   <title="Arbre ascendant d'Eugénie Collet">
   <index="Collet, Eugénie (ep Vaillant)">
   <href="http://127.0.0.1:2317/Chausey?m=A;p=eugenie (x);n=collet;v=5;siblings=on;notes=on;t=T;after=;before=;dag=on;templ=tex;w=hg:1045">
   <end>
*)

let skip_m_cmd = [ "MOD_NOTES" ]
let one_page och line = output_string och line

(* process_tree_cumul accumulates results in a string *)

let rec process_tree_cumul och cumul tree (row, col) =
  let tag_a cumul _name attributes children =
    (* if p <> "" or n <> "" we have a person *)
    (* it may appear undes a different spelling as part of <a>xxx</a> *)
    (* or we may have a portrait k <> "" *)
    (* or an image m=IM|IMH|DOC|SRC *)
    (* <a href=m=IM...k=p.oc.n><img src=m=IM...k=p.oc.n></a> *)
    (* assumes that children is a single string *)
    (* which may be an <img src=xxx> *)
    (* TODO m=TT t=xxx p=yyy *)
    (* TODO decode names  p=louis;n=de%20bourbon; *)
    if !level = 1 then Printf.eprintf "Tag a (cumul)\n";
    let attr = get_att_list attributes in
    let href = try List.assoc "href" attr with Not_found -> "" in
    let href = decode href |> escape in
    let b, m, p, n, oc, i, k, s, _v = split_href href in
    if !level = 1 then (
      Printf.eprintf "Tag a: %d, %s\n" (List.length children) href;
      dump children 0);
    if List.mem m skip_m_cmd then cumul
    else
      let content =
        List.fold_left
          (fun acc c -> acc ^ process_tree_cumul och cumul c (row, col))
          "" children
      in
      let ip =
        match
          Gwdb.person_of_key !my_base p n
            (try int_of_string oc with Failure _ -> 0)
        with
        | Some ip -> ip
        | None -> (
            try Gwdb.iper_of_string i with Failure _ -> Gwdb.dummy_iper)
      in
      let str =
        let person = Gwdb.poi !my_base ip in
        let fn = Gwdb.sou !my_base (Gwdb.get_first_name person) in
        let sn = Gwdb.sou !my_base (Gwdb.get_surname person) in
        let ocn = try Gwdb.get_occ person with Failure _ -> 0 in
        let ocn = if ocn = 0 then "" else Format.sprintf " (%d)" ocn in
        (* TODO verify uppercase! (le Fort), (Le Fort) *)
        let check = Printf.sprintf "%s %s%s" fn sn ocn in
        if !level = 1 then Printf.eprintf "Check: (%s), (%s)\n" content check;
        if (fn <> "" || sn <> "") && k = "" then
          Format.sprintf "{\\bf %s}" content
          ^ Format.sprintf "\\index{%s, %s%s}" sn fn ocn
          ^
          if check <> content then
            Format.sprintf "\\index{%s, voir %s, %s%s}" content sn fn ocn
          else ""
        else if b <> !family then Format.sprintf "%s\\footnote{%s}" content href
        else if s <> "" then (
          incr image_nbr;
          let image =
            ( Images,
              s,
              (!chapter, !section, !subsection, !subsubsection),
              !image_nbr )
          in
          if !collect_images then images_in_page := image :: !images_in_page;
          let str =
            match !image_label with
            | 4 ->
                Format.sprintf "%s\\textsuperscript{%d.%d.%d.%d}" content
                  !chapter !section !subsection !image_nbr
            | _ ->
                Format.sprintf "%s\\textsuperscript{%d.%d.%d}" content !chapter
                  !section !image_nbr
          in
          str)
        else content
      in
      str
  in

  let tag_img _cumul _name attributes _children =
    if !level = 1 then Printf.eprintf "Tag img (cumul)\n";
    let attr = get_att_list attributes in
    let href = try List.assoc "src" attr with Not_found -> "" in
    let href = decode href |> escape in
    let _b, _m, p, n, oc, i, k, s, _v = split_href href in
    let ip =
      match
        Gwdb.person_of_key !my_base p n
          (try int_of_string oc with Failure _ -> 0)
      with
      | Some ip -> ip
      | None -> if i = "" then Gwdb.dummy_iper else Gwdb.iper_of_string i
    in
    let str =
      let person = Gwdb.poi !my_base ip in
      let fn = Gwdb.sou !my_base (Gwdb.get_first_name person) in
      let sn = Gwdb.sou !my_base (Gwdb.get_surname person) in
      let ocn = try Gwdb.get_occ person with Failure _ -> 0 in
      let image_label = Format.sprintf "%s.%d.%s" fn ocn sn in
      let image =
        ( (if k <> "" then Imagek else if s <> "" then Images else Portrait),
          (if k <> "" then image_label else s),
          (!chapter, !section, !subsection, !subsubsection),
          !image_nbr )
      in
      incr image_nbr;
      if !collect_images then images_in_page := image :: !images_in_page;
      print_image image
    in
    str
  in

  let get_child children =
    List.fold_left
      (fun acc c -> acc ^ process_tree_cumul och "" c (row, col))
      "" children
  in

  let get_attr attributes attr =
    List.fold_left
      (fun c ((_, k), v) -> if k = attr then v ^ c else c)
      "" attributes
  in

  let test_attr attributes attr value =
    List.exists (fun ((_, k), v) -> k = attr && v = value) attributes
  in

  let continue cumul children =
    List.fold_left
    (fun acc c -> acc ^ process_tree_cumul och cumul c (row, col))
    cumul children
  in

  let suppress_multiple_sp str =
    let b = Buffer.create 100 in
    let rec loop cond i =
      if i = String.length str then Buffer.contents b
      else if str.[i] <> ' ' then (Buffer.add_char b str.[i]; loop true (i + 1))
      else if cond then (Buffer.add_char b str.[i]; loop false (i + 1))
      else loop false (i + 1)
    in loop true 0;
  in
  
  let clean_double_back_slash str =
    let s =
      let rec loop s =
        let i = try (String.rindex s '\\') with Not_found -> -1 in
        if i = -1 then s
        else (
          if !level = 10 then
            Printf.eprintf "Str: (%d) (%d) %s [%c|%c|%c]\n" i (String.length s) s s.[i - 1] s.[i] s.[i + 1];
          if (String.length s) > (i + 2) && s.[i - 1] = '\\' then (
          if !level = 10 then
            Printf.eprintf "Sub: (%s) (%s)\n" (String.sub s 0 (i - 2)) (String.sub s (i + 1) (String.length s - i - 2));
          loop ((String.sub s 0 (i - 2)) ^ (String.sub s (i + 1) (String.length s - i - 2))))
          else s)
      in loop str
    in
    let s = replace '\n' ' ' s in
    let s = suppress_multiple_sp s in
    strip_all_trailing_spaces s
  in

  let print_tree new_tree =
    let tree, _n =
      (List.fold_left (fun (acc1, r) row ->
        let str =
          List.fold_left (fun acc2 (_, s, ty, te, it) ->
            let cell =
              (match ty with
              | "Te" -> "Te: " ^ (clean_double_back_slash te)
              | "It" -> "It: " ^ (clean_double_back_slash it)
              | "Hl" -> "Hr: " ^ "-l"
              | "Hr" -> "Hr: " ^ "r-"
              | "Hc" -> "Hr: " ^ "--"
              | "Hv" -> "Vr: " ^ "|"
              | "E" ->  "E: " ^ ""
              | "Im" -> "Im: " ^ "Image"
              | _ -> "x" ) ^ (if s > 1 then (Format.sprintf "(%d)" s) else "")
            in
              acc2 ^ "[" ^ cell ^ "] "
          ) "" row
        in
        (acc1 ^ (Format.sprintf "Row %d: (%d) %s\\\\\n" r (List.length row) str), (r + 1))
      ) ("", 1) new_tree)
    in
    Format.sprintf "Interim print\\\\\n %s\n" tree
  in

  if !level = 2 then Printf.eprintf "Process tree cumul\n";
  match tree with
  | Text s ->
      if !level = 1 then Printf.eprintf "Text elt: %s\n" s;
      cumul ^ s
  | Element (name, attributes, children) as elt -> (
      if !level = -1 then dump_tag elt;
      if !level = 2 then Printf.eprintf "Tag elt: %s\n" name;
      match name with
      | ("i" | "u" | "b" | "em" | "tt" | "strong" | "tiny" | "small" | "big" ) as t ->
          let content = get_child children in
          cumul ^ simple_tag_1 t content
      | "br" -> cumul ^ " \\\\\n"
      | "sup" ->
          let content = get_child children in
          cumul
          ^
          if content <> "" then Format.sprintf "\\textsuperscript{%s}" content
          else ""
      | "h1" ->
          let content = get_child children in
          cumul
          ^
          if content <> "" && !section_on_a_tag then
            Format.sprintf "\\section{%s}" content
          else ""
      | "h2" ->
          let content = get_child children in
          cumul
          ^
          if content <> "" then Format.sprintf "\\subsection{%s}" content
          else ""
      | "h3" ->
          let content = get_child children in
          let str =
            if contains content ">Bateaux<" then "\n\\par\\hgbato{Bateaux}"
            else if contains content ">Propriétaires<" then
              "\n\\par\\hgbato{Propriétaires}"
            else Format.sprintf "\\subsubsection{%s}" content
          in
          cumul ^ if content <> "" then str else ""
      | "hr" -> cumul ^ "\\par\\noindent\\rule{\\textwidth}{0.4pt}\n"
      | "p" ->
          let content = get_child children in
          cumul
          ^ if content <> "" then Format.sprintf "\\par\n %s" content else ""
(*     | "table" ->
          if !level = 5 then dump_tag elt;
          let content = get_child children in
          cumul ^ content
      | "caption" ->
          let content = get_child children in
          let content = Format.sprintf "\n\\caption{%s}\n" content in
          cumul ^ content
      | "tbody" ->
          (* implicit if no <caption> or <thead> *)
          if !level = 3 then Printf.eprintf "Table: begin\n";
          let content = get_child children in
          if !level = 3 then Printf.eprintf "Table: %s\n" content;
          (* TODO compute the number of columns and their style *)
          let cols =
            let rec loop s n =
              if n = !td_nbr then s else loop (s ^ "l") (n + 1)
            in
            loop "" 0
          in
          let content =
            Format.sprintf "\n\\begin{tabular}{%s}\n%s\n\\end{tabular}\n" cols
              content
          in
          first_tr := true;
          cumul ^ content
      | "tr" ->
          if !level = 3 then Printf.eprintf "Tr:\n";
          let content = get_child children in
          if !level = 3 then Printf.eprintf "Tr: %s\n" content;
          first_tr := false;
          first_td := true;
          cumul ^ content ^ "\\\\\n"
      | "td" ->
          let first = !first_td in
          if !level = 3 then Printf.eprintf "Td:\n";
          let content = get_child children in
          if !level = 3 then Printf.eprintf "Td: %s\n" content;
          let colspan =
            List.fold_left
              (fun c ((_, k), v) ->
                if k = "colspan" then try int_of_string v with Failure _ -> 1
                else c)
              1 attributes
          in
          let content =
            if colspan > 1 then
              Format.sprintf "\\multicolumn{%d}{}{%s}" colspan content
            else content
          in
          first_td := false;
          if !first_tr then incr td_nbr;
          cumul ^ (if first then "" else " & ") ^ content *)
      | "ul" ->
          let content = get_child children in
          cumul
          ^
          if content <> "" then
            Format.sprintf "\\begin{hgitemize}\n %s\n\\end{hgitemize}\n" content
          else ""
      | "li" ->
          let content = get_child children in
          cumul
          ^ if content <> "" then Format.sprintf "\\item{}{%s}" content else ""
      | "span" ->
          (* look for potential TeX code *)
          (* <span style="display:none">tex \index%{Gelin, Zacharie}tex</span> *)
          (* or \highlight *)
          (* <span mode="highlight">sn fn%if;(oc != "0") (oc)%end;</span> *)
          (* children is a single string of TeX *)
          (* % might not be there (old format) *)
          if !level = 4 then Printf.eprintf "Span %d\n" (List.length attributes);
          if !level = 4 then
            List.iter
              (fun ((_, k), v) -> Printf.eprintf "Kv: %s=(%s)\n" k v)
              attributes;
          let display_none = test_attr attributes "style" "display:none" in
          let highlight_mode = test_attr attributes "mode" "highlight" in
          let tex_mode_1 = test_attr attributes "mode" "tex" in
          let str =
            let content = get_child children in
            let tex_mode_2 =
              if String.length content > 3 then String.sub content 0 3 = "tex"
              else false
            in
            if !level = 4 then Printf.eprintf "Content (span): %s\n" content;
            if display_none then
              if String.length content > 3 && (tex_mode_1 || tex_mode_2) then (
                let content = replace '[' '{' content in
                let content = replace ']' '}' content in
                if !level = 1 then Printf.eprintf "TeX content: %s\n" content;
                let content =
                  if tex_mode_2 then
                    String.sub content 4 (String.length content - 7)
                  else content
                in
                let i =
                  try String.index_from content 0 '%' with Not_found -> -1
                in
                if i > 0 then
                  String.sub content 0 i
                  ^ String.sub content (i + 1) (String.length content - i - 1)
                else content)
              else content
            else if highlight_mode then (
              if !level = 2 then Printf.eprintf "Hl2: (%s)\n" content;
              if List.mem content !highlights then
                Format.sprintf "{\\hl{\\bf %s}}" content
              else Format.sprintf "{\\bf %s}" content)
            else content
          in
          cumul ^ str ^ "\n"
      | "div" ->
          (* <div class="container" style="column-count:2;column-gap:50px"> *)
          (* <div class="column"> *)
          (* <div class="row"> *)
          let clas = get_attr attributes "class" in
          if !level = 6 then Printf.eprintf "Div: %s\n" clas;
          let sty = get_attr attributes "style" in
          let sty = String.split_on_char ';' sty in
          let sty =
            List.map
              (fun s ->
                let p = String.split_on_char ':' s in
                let len = List.length p in
                ( (if len > 0 then List.nth p 0 else ""),
                  if len > 1 then List.nth p 1 else "" ))
              sty
          in
          let cols =
            try
              try int_of_string (List.assoc "column-count" sty)
              with Failure _ -> 0
            with Not_found -> 0
          in
          if !level = 6 then Printf.eprintf "Columns: %d, %d\n" cols col;
          let tabl = if contains clas "columns" then (0, 0) else (row, col) in
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c tabl)
              "" children
          in
          if contains clas "container" && cols > 1 then
            cumul
            ^ Format.sprintf "\n\\begin{multicols}{%d}\n%s\n\\end{multicols}\n"
                cols content
          else cumul ^ content
      (* Trees ********************************* *)
      | "bigtree" ->
          if !level = 8 then (
            Printf.eprintf "Trees:\n";
            dump_tag elt);
          new_tree := [ [] ];
          new_row := [];
          let _ = continue "" children in
          if !new_row <> [] then new_tree := List.rev !new_row :: !new_tree;
          if !level = 8 then (
            let wid, len =
              List.fold_left (fun (w0, l) row ->
                let w1 =
                  List.fold_left (fun w (_, s, _, _, _) -> (w + s)) 0 row
                in
                if l <> 0 then assert (w1 = w0); (w1, l + 1)) (0, 0) !new_tree
            in
            Printf.eprintf "End big tree: wid=%d x len=%d\n" wid len);
          cumul ^ (print_tree (List.rev !new_tree))
      | "cell" ->
          if !level = 9 then
            Printf.eprintf "Cell: (%d) %s %s %s\n" !c_span !c_typ !c_txt !c_item;
          if !c_typ <> "" then
            new_row := (!c_width, !c_span, !c_typ, !c_txt, !c_item) :: !new_row;
          c_width := 0;
          let span = get_attr attributes "colspan" in
          (c_span := try int_of_string span with Failure _ -> 1);
          c_typ := "";
          c_txt := "";
          c_item := "";
          continue "" children
      | "celltext" ->
          c_typ := "Te"; (* TODO escape & *)
          c_txt := escape (get_child children);
          if !level = 9 then Printf.eprintf "Cell txt: %s\n" !c_txt;
          continue "" children
      | "cellitem" ->
          c_typ := "It";
          c_item := escape (get_child children);
          if !level = 9 then Printf.eprintf "Cell item: %s\n" !c_item;
          continue "" children
      | "rule-left" ->
          c_typ := "Hl";
          continue "" children
      | "rule-right" ->
          c_typ := "Hr";
          continue "" children
      | "rule-fullcell" ->
          c_typ := "Hc";
          continue "" children
      | "vbar" ->
          c_typ := "Hv";
          continue "" children
      | "emptycell" ->
          c_typ := "E";
          continue "" children
      | "newline" ->
          new_tree := List.rev !new_row :: !new_tree;
          new_row := [];
          continue "" children
      | "image" ->
          let img = get_child children in
          c_typ := "Im";
          c_txt := !c_txt ^ img;
          continue "" children
      (* end trees ***************************** *)
      | name when List.mem name dummy_tags_0 ->
          continue cumul children
      | name when List.mem name dummy_tags_1 -> cumul
      | name when List.mem name dummy_tags_2 -> cumul
      | name when List.mem name dummy_tags_3 -> cumul
      (* cumul is handled by these two tag functions *)
      | "a" -> tag_a cumul name attributes children
      | "img" -> tag_img cumul name attributes children
      | _ ->
          Printf.eprintf "Missing tag: %s\n" name;
          continue cumul children)

let process_html och body =
  let open Markup in
  let tree =
    body |> string |> parse_html |> signals
    |> tree
         ~text:(fun ss -> Text (String.concat "" ss))
         ~element:(fun (_, name) attributes children ->
           Element (name, attributes, children))
  in
  let content =
    match tree with
    | Some tree -> process_tree_cumul och "" tree (0, 0)
    | _ -> "bad tree"
  in
  output_string och content

let bad_code c = c >= 400

(* <x Cmd param>remain *)
(*       i     j       *)
let one_command och line =
  let line = String.sub line 0 (String.length line - 1) in
  let i =
    try String.index_from line 3 ' ' with Not_found -> String.length line - 1
  in
  let j =
    try String.index_from line 0 '>' with Not_found -> String.length line - 1
  in
  let cmd = if i > 0 then String.sub line 3 (i - 3) else "" in
  let param =
    if i > 0 && i < String.length line - 1 && j > 0 && j < String.length line
    then String.sub line (i + 1) (j - i - 1)
    else ""
  in
  let remain =
    if j > 0 && j < String.length line - 1 then
      String.sub line (j + 1) (String.length line - j - 1)
    else ""
  in
  let out c param =
    output_string och (Format.sprintf "\\%s{%s}%s\n" c param remain)
  in
  match cmd with
  | "Adjust_w" ->
      output_string och
        (Format.sprintf "Newwidth {\\bf %s}\n" (* TODO *)
           (if param <> "" then param else "7cm"))
  | "Arbres" -> arbres := param = "on"
  | "Chapter" ->
      out "chapter" param;
      incr chapter;
      current_level := 0;
      section := 0;
      subsection := 0;
      subsubsection := 0
  | "Ch_nb_in_fig_nb" -> ch_nb_in_fig_nb := param = "on"
  | "CollectImages" -> collect_images := param = "on"
  | "Ep" -> ep := param = "on"
  | "Fiches" -> section_on_a_tag := param = "on"
  | "HighLight" ->
      highlights := param :: !highlights;
      if !level = 2 then
        List.iter (fun hl -> Printf.eprintf "Hl1: (%s)\n" hl) !highlights
  | "ImageLabel" -> (
      image_label := try int_of_string param with Failure _ -> 3)
  | "LaTeX" -> output_string och param
  | "Newpage" -> output_string och "\\newpage"
  | "Section" ->
      out "section" param;
      incr section;
      current_level := 1;
      subsection := 0;
      subsubsection := 0
  | "Sub" -> sub := param = "on"
  | "SubSection" ->
      out "subsection" param;
      incr subsection;
      current_level := 2;
      subsubsection := 0
  | "SubSubSection" ->
      out "subsubsection" param;
      incr subsubsection;
      current_level := 3
  | "Version" -> output_string och (version ^ "\n")
  | "Wide" -> wide := param = "on"
  | "Width" -> im_width := param
  | _ -> output_string och (Format.sprintf "%%%s%s\n" cmd remain)

let one_http_call och line =
  let url_beg = try String.index_from line 0 '"' with Not_found -> 0 in
  let url_end =
    try String.index_from line (url_beg + 1) '"' with Not_found -> 0
  in
  let url = String.sub line (url_beg + 1) (url_end - url_beg - 1) in
  let resp = Ezcurl.get ~url () in
  match resp with
  | Ok { Ezcurl.code; body; _ } ->
      if bad_code code then (
        Printf.eprintf "bad code when fetching %s: %d\n%!" url code;
        output_string och
          (Format.sprintf "Bad code when fetching %s: %d!\n" url code))
      else (
        if !level = 1 then Printf.eprintf "Body size: %d\n" (String.length body);
        let _ = process_html och body in
        ())
  | Error (_, msg) ->
      Printf.eprintf "error when fetching %s:\n  %s\n%!" url msg;
      output_string och
        (Format.sprintf "Error when fetching %s:\n %s\n" url msg)

let print_images och images_list =
  output_string och (Format.sprintf "\\par\n");
  (* TODO manage Wide *)
  List.iter
    (fun (im_type, name, (ch, sec, _ssec, _sssec), nbr) ->
      match im_type with
      | Imagek | Portrait -> ()
      | Images ->
          let name = Filename.remove_extension name in
          let images_dir =
            "/Users/Henri/Genea/GeneWeb-Bases/src/chausey/images"
          in
          output_string och
            (Format.sprintf
               "\\parbox{%s}{\\includegraphics[width=%s]{%s/%s}\\\\\\hglabxa{%d}{%d}{%d}}\n"
               !im_width !im_width images_dir name ch sec nbr))
    (List.rev images_list);
  output_string och (Format.sprintf "\\par\n")

let process_one_line och line =
  match line.[0] with
  | '<' -> (
      match line.[1] with
      | 'a' ->
          let content = get_a_content line in
          let sec =
            match !current_level with
            | 0 -> ""
            | 1 -> "sub"
            | 2 -> "subsub"
            | _ -> "subsubsub"
          in
          let index =
            if String.contains line '\\' then ""
            else Format.sprintf "\\index{%s}" content
          in
          output_string och
            (Format.sprintf "\\%ssection{%s%s}\n" sec content index);
          one_http_call och line;
          if !collect_images && !images_in_page <> [] then
            print_images och !images_in_page;
          images_in_page := [];
          output_string och (Format.sprintf "\\hrule\n")
      | 'b' -> one_page och line
      | 'x' -> one_command och line
      | 'y' -> output_string och ""
      | _ -> output_string och (line ^ "\n"))
  | _ -> output_string och (line ^ "\n")

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> base := x), " Choose base.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ( "-o",
        Arg.String (fun x -> out_file := x),
        " Name of the result (default family.gw2l)." );
      ( "-index",
        Arg.Int (fun x -> index := x),
        " Number of times makeindex is done." );
      ("-level", Arg.Int (fun x -> level := x), " Debug traces level.");
      ("-verbose", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
      ("-v", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
      ("-follow", Arg.Set follow, " Produce Pdflatex.");
      ( "-test",
        Arg.Int
          (fun x ->
            test_nb := x;
            test := true),
        " Choose test file." );
      ("-debug", Arg.Unit (fun () -> debug := true), " Debug mode.");
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;
  (* install tex templates in bases/etc *)
  let etc_dir = Filename.concat !bases "etc" in
  let cmmd = Format.sprintf "cp -R ./tex %s" etc_dir in
  let error = Sys.command cmmd in
  if error > 0 then (
    Printf.eprintf "Error while loading tex templates files (%d)\n" error;
    exit 0);
  (if !family <> "" then
   let cmmd = Format.sprintf "cp -R ./%s.gwb ./%s-tmp.gwb" !family !family in
   let error = Sys.command cmmd in
   if error > 0 then
     Printf.eprintf "Error while copying base folder (%d)\n" error);
  let fname_txt, family_out =
    ( (if !family <> "" then !family ^ ".txt"
      else Printf.sprintf "test/gwtolatex-test%d.txt" !test_nb),
      if !family <> "" then !family
      else Printf.sprintf "gwtolatex-test%d" !test_nb )
  in
  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_all = Filename.concat !livres (!family ^ ".txt") in
  let fname_out =
    if !out_file <> "" then !out_file
    else Filename.concat "livres" (family_out ^ ".tex")
  in
  let mode, fname_in, och =
    if Sys.file_exists fname_txt then
      ("txt", fname_txt, if !follow then open_out fname_out else stderr)
    else if Sys.file_exists fname_htm then ("html", fname_htm, stderr)
    else ("", fname_all, open_out fname_out)
  in
  let ic = open_in_bin fname_in in
  if not !debug then Sys.enable_runtime_warnings false;
  Printf.eprintf "\nThis is GwToLaTeX version %s on %s (%d)\n" version fname_in
    !level;
  flush stderr;
  (match mode with
  | "html" ->
      let body = really_input_string ic (in_channel_length ic) in
      let _ = process_html och body in
      close_in ic;
      close_out och;
      exit 0
  | _ -> (
      try
        while true do
          let line = input_line ic in
          process_one_line och line
        done
      with End_of_file ->
        close_in ic;
        close_out och));
  Printf.eprintf "Done txt parsing\n";
  flush stderr;

  let mode = if !verbose then "" else "-interaction=batchmode" in
  let cmmd1 =
    Printf.sprintf "pdflatex -output-directory=livres %s %s.tex" mode family_out
  in
  Printf.eprintf "First pass at pdflatex \n";
  let error = Sys.command cmmd1 in
  if error <> 0 then (
    Printf.eprintf "Error in pdflatex processing (%d)\n" error;
    exit 0);
  Printf.eprintf "Building index\n";
  if !test && !index = 0 then exit 0;
  (* makeindex does not like absolute paths! *)
  let cmmd2 =
    Printf.sprintf "makeindex livres%s%s.idx" Filename.dir_sep family_out
  in
  for _i = 0 to !index do
    let error = Sys.command cmmd2 in
    if error <> 0 then (
      Printf.eprintf "Error in makeindex processing (%d)\n" error;
      exit 0)
  done;
  Printf.eprintf "Second pass at pdflatex \n";
  let error = Sys.command cmmd1 in
  if error <> 0 then (
    Printf.eprintf "Error in 2nd pdflatex processing (%d)\n" error;
    exit 0);
  Printf.eprintf "Done all\n";
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
