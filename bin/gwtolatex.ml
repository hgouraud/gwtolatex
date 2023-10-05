(* Copyright (c) 2013 H.Gouraud *)

type name = string * string

type my_tree =
  | Text of string
  | Element of string * (name * string) list * my_tree list

type im_type = Portrait | Image_k | Image_s

type _image = {
  im_type : im_type;
  filename : string;
  where : int * int * int * int; (* ch, sec, ssec, sssec*)
  image_nbr : int;
}

let test = ref false
let follow = ref false
let test_nb = ref 0
let level = ref 1
let version = "1.0"

(* current values *)
let chapter = ref 0
let section = ref 0
let subsection = ref 0
let subsubsection = ref 0
let nbr = ref 0
let images_in_page = ref []
let chapter = ref 0
let section = ref 0
let subsection = ref 0
let subsubsection = ref 0
let image_nbr = ref 0
let images_in_page = ref []
let collect_images = ref false
let wide = ref false
let fiches = ref false
let highlights = ref []
let width = ref 7
let ch_nb_in_fig_nb = ref true
let immediate = ref false
let sub = ref false
let trees = ref false
let ep = ref false
let arbres = ref false
let sideways = ref false

let open_base basename =
  match basename with
  | "" ->
      Printf.eprintf "No basename supplied\n";
      exit 1
  | bfile -> (
      let base = try Some (Gwdb.open_base (bfile ^ ".gwb")) with _ -> None in
      match base with
      | None ->
          Printf.eprintf "Cannot open base %s\n" bfile;
          exit 1
      | Some base -> base)

let my_base = ref (open_base "./chausey")

let _strip_nl s =
  let b = Buffer.create 10 in
  String.iter
    (fun c -> if c = '\n' then Buffer.add_char b ' ' else Buffer.add_char b c)
    s;
  Buffer.contents b

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
  if !level > 1 then
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
        if !level > 1 then
          Printf.eprintf "Found: %s, (%d)\n"
            (if found then "yes" else "no")
            (String.length body);
        if found then (
          (* <tag>content</tag>, body *)
          let content = String.sub body 0 j in
          if !level > 1 then
            Printf.eprintf "Body3: name: %s, %d (%s) (%s)\n" name j
              (chop_body 30 body) content;
          let body =
            String.sub body
              (j + 3 + String.length name)
              (String.length body - (j + 3 + String.length name))
          in
          if !level > 1 then Printf.eprintf "Body4: (%s)\n" (chop_body 30 body);
          let body =
            if body <> "" && body.[0] = '\n' then
              String.sub body 1 (String.length body - 1)
            else body
          in
          if !level > 1 then Printf.eprintf "Body5: (%s)\n" (chop_body 30 body);
          (content, body))
        else match_tag (j + 1) body)
      else match_tag (j + 1) body
  in
  match_tag 0 body

(* read children, ignore tag *)
let dummy_tags_0 = [ "body"; "html"; "div" ]
let dummy_tags_1 = [ "!--"; "bdo"; "samp"; "span"; "table"; "tbody" ]

(* ignore tag *)
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

(* skip to end tag *)
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

let rec dump children l =
  let tab l =
    let rec loop acc i = if i = l then acc else loop (acc ^ "..") (i + 1) in
    loop ".." 0
  in
  Printf.eprintf "%sChildren: %d\n" (tab l) (List.length children);
  List.iter
    (fun elt ->
      match elt with
      | Text t -> Printf.eprintf "%sTxt: %s\n" (tab l) t
      | Element (n, a, c) ->
          Printf.eprintf "%sElt: %s\n" (tab l) n;
          dump c (l + 1))
    children;
  Printf.eprintf "End children:\n"

let dump_tag tag name attributes children =
  Printf.eprintf "..<begin %s\n>" name;
  List.iter (fun ((a, b), c) -> Printf.eprintf "  attr: %s=%s\n" b c) attributes;
  dump children 0;
  Printf.eprintf "..<end %s>" name

(* in str, replace car x by car y *)
let replace x y str =
  let b = Buffer.create 40 in
  String.iter
    (fun c -> if c = x then Buffer.add_char b y else Buffer.add_char b c)
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
  if !level > 1 then
    List.iter (fun (k, v) -> Printf.eprintf "%s=%s\n" k v) evars;
  let m = try List.assoc "m" evars with Not_found -> "" in
  let p = try List.assoc "p" evars with Not_found -> "" in
  let n = try List.assoc "n" evars with Not_found -> "" in
  let oc = try List.assoc "oc" evars with Not_found -> "" in
  let i = try List.assoc "i" evars with Not_found -> "" in
  let k = try List.assoc "k" evars with Not_found -> "" in
  let s = try List.assoc "s" evars with Not_found -> "" in
  let v = try List.assoc "v" evars with Not_found -> "" in
  (m, p, n, oc, i, k, s, v)

let print_image image =
  match image with
  | ((Portrait | Image_s | Image_k) as t), name, (ch, sec, ssec, sssec), nb ->
      Format.sprintf "Type: %s, name: %s, (%d, %d, %d, %d), nb: %d"
        (match t with
        | Portrait -> "Portrait"
        | Image_s -> "Image_s"
        | Image_k -> "Image_k")
        name ch sec ssec sssec nb

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

let one_page och line = output_string och line

(* process_tree_cumul accumulates results in a string *)

let rec process_tree_cumul och cumul tree =
  let tag_a cumul name attributes children =
    if !level > 1 then Printf.eprintf "Tag a (cumul)\n";
    let attr = get_att_list attributes in
    let href = try List.assoc "href" attr with Not_found -> "" in
    let m, p, n, oc, i, k, s, v = split_href href in
    if !level > 1 then (
      Printf.eprintf "Tag a: %d, %s\n" (List.length children) href;
      dump children 0);
    let content =
      List.fold_left (fun acc c -> process_tree_cumul och cumul c) "" children
    in
    let ip =
      match
        Gwdb.person_of_key !my_base p n
          (try int_of_string oc with Failure _ -> 0)
      with
      | Some ip -> ip
      | None -> Gwdb.iper_of_string i
    in
    let str =
      let person = Gwdb.poi !my_base ip in
      let fn = Gwdb.sou !my_base (Gwdb.get_first_name person) in
      let sn = Gwdb.sou !my_base (Gwdb.get_surname person) in
      let ocn = try Gwdb.get_occ person with Failure _ -> 0 in
      let ocn = if ocn = 0 then "" else Format.sprintf "(%d)" ocn in
      let check = Printf.sprintf "%s %s" fn sn in
      if !level > 1 then Printf.eprintf "Check: (%s), (%s)\n" content check;
      if (p <> "" || n <> "") && k = "" && content <> check then
        Format.sprintf "{\\b %s %s %s}" fn sn ocn
        ^ Format.sprintf "\\index{%s, %s %s}" sn fn ocn
        ^ Format.sprintf "\\index{%s, voir %s, %s %s\n}" content sn fn ocn
      else ""
    in
    str ^ content
  in

  let tag_img cumul name attributes children =
    if !level > 1 then Printf.eprintf "Tag img (cumul)\n";
    let attr = get_att_list attributes in
    let href = try List.assoc "src" attr with Not_found -> "" in
    let m, p, n, oc, i, k, s, v = split_href href in
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
        ( (if k <> "" then Image_k else if s <> "" then Image_s else Portrait),
          image_label,
          (!chapter, !section, !subsection, !subsubsection),
          !image_nbr )
      in
      incr nbr;
      if !collect_images then images_in_page := image :: !images_in_page;
      print_image image
    in
    str
  in

  match tree with
  | Text s -> cumul ^ s
  | Element (name, attributes, children) -> (
      match name with
      | ("i" | "b" | "u" | "em") as t ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^ if content <> "" then Format.sprintf "{\\%s %s}" t content else ""
      | "br" -> "\\\n"
      | "sup" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^
          if content <> "" then Format.sprintf "\\textsuperscript{%s}" content
          else ""
      | "h1" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          let sect =
            if !subsection = 1 then (
              incr subsection;
              subsubsection := 0;
              "subsection")
            else (
              incr section;
              subsection := 0;
              subsection := 0;
              subsubsection := 0;
              "section")
          in
          cumul
          ^
          if content <> "" then Format.sprintf "{\\%s %s}" sect content else ""
      | "h2" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^
          if content <> "" then Format.sprintf "{\\subsection %s}" content
          else ""
      | "h3" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          let str =
            if contains content ">Bateaux<" then "\n\\par\\hgbato{Bateaux}"
            else if contains content ">Propriétaires<" then
              "\n\\par\\hgbato{Propriétaires}"
            else Format.sprintf "{\\subsubsection %s}" content
          in
          cumul ^ if content <> "" then str else ""
      | "p" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^ if content <> "" then Format.sprintf "\\par\n %s" content else ""
      | "ul" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^
          if content <> "" then
            Format.sprintf "\\begin{hgitemise}\n %s/n\\end{hgitemise}\n" content
          else ""
      | "li" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^ if content <> "" then Format.sprintf "\\item{}{%s}" content else ""
      | "small" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          cumul
          ^ if content <> "" then Format.sprintf "{\\small %s}" content else ""
      | "span" ->
          (* look for potential TeX code *)
          (* <span style="display:none">tex \index%{Gelin, Zacharie}tex</span> *)
          (* children is a single string of TeX *)
          let tex =
            List.fold_left
              (fun ok ((_, k), v) ->
                if k = "class" && v = "display:none" then ok || true
                else ok || false)
              false attributes
          in
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och cumul c)
              "" children
          in
          let str =
            if tex && String.sub content 0 3 = "tex" then (
              if !level > 1 then Printf.eprintf "TeX content: %s\n" content;
              let content = String.sub content 4 (String.length content - 7) in
              let content =
                let i =
                  try String.index_from content 0 '%' with Not_found -> -1
                in
                if i > 0 then
                  String.sub content 0 i
                  ^ String.sub content (i + 1) (String.length content - i - 1)
                else content
              in
              content)
            else content
          in
          cumul ^ str
      | name when List.mem name dummy_tags_1 -> ""
      | name when List.mem name dummy_tags_2 -> ""
      | name when List.mem name dummy_tags_3 -> ""
      | "a" -> tag_a cumul name attributes children
      | "img" -> tag_img cumul name attributes children
      | _ ->
          Printf.eprintf "Missing tag: %s\n" name;
          "")

let rec process_tree och tree =
  let tag_a name attributes children =
    (* if p <> "" or n <> "" we have a person *)
    (* it may appear undes a different spelling as part of <a>xxx</a> *)
    (* or we may have a portrait k <> "" *)
    (* or an image m=IM|IMH|DOC|SRC *)
    (* <a href=m=IM...k=p.oc.n><img src=m=IM...k=p.oc.n></a> *)
    (* assumes that children is a single string *)
    (* which may be an <img src=xxx> *)
    (* TODO m=TT t=xxx p=yyy *)
    (* TODO decode names  p=louis;n=de%20bourbon; *)
    if !level > 1 then Printf.eprintf "Tag a (tree)\n";
    let attr = get_att_list attributes in
    let href = try List.assoc "href" attr with Not_found -> "" in
    let m, p, n, oc, i, k, s, v = split_href href in
    if !level > 1 then (
      Printf.eprintf "Tag a: %d, %s\n" (List.length children) href;
      if !level > 2 then dump children 0);
    let content =
      List.fold_left (fun acc c -> process_tree_cumul och acc c) "" children
    in
    let ip =
      if !level > 1 then Printf.eprintf "Key: %s %s %s\n" p n oc;
      match
        Gwdb.person_of_key !my_base p n
          (try int_of_string oc with Failure _ -> 0)
      with
      | Some ip -> ip
      | None -> if i = "" then Gwdb.dummy_iper else Gwdb.iper_of_string i
    in
    if !level > 1 then Printf.eprintf "Ip: %s\n" (Gwdb.string_of_iper ip);
    let str =
      let person = Gwdb.poi !my_base ip in
      let fn = Gwdb.sou !my_base (Gwdb.get_first_name person) in
      let sn = Gwdb.sou !my_base (Gwdb.get_surname person) in
      let ocn = try Gwdb.get_occ person with Failure _ -> 0 in
      let ocn = if ocn = 0 then "" else Format.sprintf "(%d)" ocn in
      (* TODO verify uppercase! (le Fort), (Le Fort) *)
      let check = Printf.sprintf "%s %s" fn sn in
      if !level > 1 then Printf.eprintf "Check: (%s), (%s)\n" content check;
      if (fn <> "" || sn <> "") && k = "" && content <> check then
        Format.sprintf "{\\b %s %s %s}" fn sn ocn
        ^ Format.sprintf "\\index{%s, %s %s}" sn fn ocn
        ^ Format.sprintf "\\index{%s, voir %s, %s %s}" content sn fn ocn
      else ""
    in
    output_string och (str ^ content)
  in

  let tag_img name attributes children =
    if !level > 1 then Printf.eprintf "Tag img (tree)\n";
    let attr = get_att_list attributes in
    let href = try List.assoc "src" attr with Not_found -> "" in
    let m, p, n, oc, i, k, s, v = split_href href in
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
        ( (if k <> "" then Image_k else if s <> "" then Image_s else Portrait),
          image_label,
          (!chapter, !section, !subsection, !subsubsection),
          !nbr )
      in
      incr nbr;
      images_in_page := image :: !images_in_page;
      print_image image
    in
    output_string och str
  in

  if !level > 1 then Printf.eprintf "Process_tree\n";
  match tree with
  | Text s ->
      if !level > 1 then Printf.eprintf "Text elt: %s\n" s;
      output_string och s
  | Element (name, attributes, children) -> (
      if !level > 1 then Printf.eprintf "Tag elt: %s\n" name;
      match name with
      | ("i" | "b" | "u" | "em") as t ->
          output_string och (Printf.sprintf "{\\%s " t);
          List.iter (fun c -> process_tree och c) children;
          output_string och "}"
      | "br" -> output_string och "\\\n"
      | "sup" ->
          output_string och (Printf.sprintf "{\\textsuperscript{");
          List.iter (fun c -> process_tree och c) children;
          output_string och "}"
      | "h1" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och acc c)
              "" children
          in
          let sect =
            if !subsection = 1 then (
              incr subsection;
              "subsection")
            else (
              incr section;
              subsection := 0;
              "section")
          in
          output_string och
            (if content <> "" then Format.sprintf "{\\%s %s}" sect content
            else "")
      | "h2" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och acc c)
              "" children
          in
          output_string och
            (if content <> "" then Format.sprintf "{\\subsection %s}" content
            else "")
      | "h3" ->
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och acc c)
              "" children
          in
          let str =
            if contains content ">Bateaux<" then "\n\\par\\hgbato{Bateaux}"
            else if contains content ">Propriétaires<" then
              "\n\\par\\hgbato{Propriétaires}"
            else Format.sprintf "{\\subsubsection %s}" content
          in
          output_string och (if content <> "" then str else "")
      | "p" ->
          output_string och "\\par\n";
          List.iter (fun c -> process_tree och c) children
      | "ul" ->
          output_string och (Format.sprintf "\\begin{hgitemise}\n");
          List.iter (fun c -> process_tree och c) children;
          output_string och "\\end{hgitemise}\n"
      | "li" ->
          output_string och (Format.sprintf "\\item{}{");
          List.iter (fun c -> process_tree och c) children;
          output_string och "}\n"
      | "small" ->
          output_string och (Format.sprintf "{\\small ");
          List.iter (fun c -> process_tree och c) children;
          output_string och "}\n"
      | "span" ->
          (* look for potential TeX code *)
          (* <span style="display:none">tex \index%{Gelin, Zacharie}tex</span> *)
          (* children is a single string of TeX *)
          let tex =
            List.exists (fun ((_, k), v) -> v = "display:none") attributes
          in
          let content =
            List.fold_left
              (fun acc c -> acc ^ process_tree_cumul och acc c)
              "" children
          in
          let str =
            if tex && String.sub content 0 3 = "tex" then (
              if !level > 1 then Printf.eprintf "TeX content: %s\n" content;
              let content = String.sub content 4 (String.length content - 7) in
              let content =
                let i =
                  try String.index_from content 0 '%' with Not_found -> -1
                in
                if i > 0 then
                  String.sub content 0 i
                  ^ String.sub content (i + 1) (String.length content - i - 1)
                else content
              in
              content)
            else content
          in
          output_string och str
      | name when List.mem name dummy_tags_0 ->
          List.iter (fun c -> process_tree och c) children
      | name when List.mem name dummy_tags_1 -> ()
      | name when List.mem name dummy_tags_2 -> ()
      | name when List.mem name dummy_tags_3 -> ()
      | "a" -> tag_a name attributes children
      | "img" -> tag_img name attributes children
      | _ -> Printf.eprintf "Missing tag: %s\n" name)

let process_html och body =
  let open Markup in
  let tree =
    body |> string |> parse_html |> signals
    |> tree
         ~text:(fun ss -> Text (String.concat "" ss))
         ~element:(fun (_, name) attributes children ->
           Element (name, attributes, children))
  in
  match tree with
  | Some tree -> process_tree och tree
  | _ -> failwith "bad tree"

let bad_code c = c >= 400

let one_command och line =
  let end_c =
    try String.index_from line 0 '>' with Not_found -> String.length line - 1
  in
  let cmd = String.sub line 3 (end_c - 3) in
  let remain =
    if end_c < String.length line then
      String.sub line (end_c + 1) (String.length line - end_c - 1)
    else ""
  in
  let out c command =
    let param =
      String.sub command (String.length c)
        (String.length command - String.length c)
    in
    output_string och (Format.sprintf "\\%s{%s}%s\n" c param remain)
  in

  let parts = String.split_on_char ' ' cmd in
  match List.nth parts 0 with
  | "Chapter" ->
      out "chapter" cmd;
      incr chapter;
      section := 0;
      subsection := 0;
      subsubsection := 0
  | "Section" ->
      out "section" cmd;
      incr section;
      subsection := 0;
      subsubsection := 0
  | "SubSection" ->
      out "subsection" cmd;
      incr subsection;
      subsubsection := 0
  | "SubSubSection" ->
      out "subsubsection" cmd;
      incr subsubsection
  | "Newpage" -> output_string och "\\newpage"
  | "Adjust_w" ->
      output_string och
        (Format.sprintf "\\newwidth{%s}\n"
           (if List.length parts > 1 then List.nth parts 0 else "7cm"))
  | "Version" -> output_string och (version ^ "\n")
  | "CollectImagesOn>" -> collect_images := true
  | "CollectImagesOff>" -> collect_images := false
  | "WideOn>" -> wide := false
  | "WideOff>" -> wide := false
  | "SubOn" -> sub := true
  | "SubOff" -> sub := false
  | "EpOn" -> ep := true
  | "EpOff" -> ep := true
  | "ArbresOn" -> arbres := true
  | "ArbresOff" -> arbres := false
  | "Ch_nb_in_fig_nb" -> ch_nb_in_fig_nb := true
  | "Ch_nb_in_fig_nbOff" -> ch_nb_in_fig_nb := false
  | "FichesOn" -> fiches := true
  | "FichesOff" -> fiches := false
  | "ImmediateOn" -> immediate := true
  | "ImmediateOff" -> immediate := false
  | _ -> output_string och (Format.sprintf "%%%s%s\n" cmd remain)

let one_http_call och line =
  let url_beg = try String.index_from line 0 '"' with Not_found -> 0 in
  let url_end =
    try String.index_from line (url_beg + 1) '"' with Not_found -> 0
  in
  let url = String.sub line (url_beg + 1) (url_end - url_beg - 1) in
  output_string och url;
  output_string och "\n";
  let resp = Ezcurl.get ~url () in
  match resp with
  | Ok { Ezcurl.code; body; _ } ->
      if bad_code code then
        Printf.eprintf "bad code when fetching %s: %d\n%!" url code
      else (
        if !level > 1 then Printf.eprintf "Body size: %d\n" (String.length body);
        let _ = process_html och body in
        ())
  | Error (_, msg) -> Printf.eprintf "error when fetching %s:\n  %s\n%!" url msg

let process_one_line och line =
  match line.[0] with
  | '<' -> (
      match line.[1] with
      | 'a' -> one_http_call och line
      | 'b' -> one_page och line
      | 'x' -> one_command och line
      | 'y' -> output_string och ""
      | _ -> output_string och (line ^ "\n"))
  | _ -> output_string och (line ^ "\n")

let base = ref ""
let family = ref ""
let out_file = ref ""
let debug = ref false
let index = ref 0
let batch = ref false
let livres = ref "/Users/Henri/Genea/Livres"
let base = ref ""
let bases = ref ""

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
      ("-level", Arg.Int (fun x -> level := x), " Test traces level.");
      ("-batch", Arg.Set batch, " Pdflatex mode (batch or not).");
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
  let fname_txt, family_out =
    ( Printf.sprintf "test/gwtolatex-test%d.txt" !test_nb,
      if !family <> "" then !family
      else Printf.sprintf "gwtolatex-test%d" !test_nb )
  in
  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_all = Filename.concat !livres (!family ^ ".txt") in
  let fname_out = if !out_file <> "" then !out_file else family_out ^ ".tex" in
  let mode, fname_in, och =
    if Sys.file_exists fname_txt then
      ("txt", fname_txt, if !follow then open_out fname_out else stderr)
    else if Sys.file_exists fname_htm then ("html", fname_htm, stderr)
    else ("", fname_all, open_out (Filename.concat !livres fname_out))
  in
  let ic = open_in_bin fname_in in
  if not !debug then Sys.enable_runtime_warnings false;
  if !test then (
    Printf.eprintf "\n***Starting test with %s\n" fname_in;
    flush stderr);
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

  let mode = if !batch then "" else "-interaction=batchmode" in
  let cmmd1 = Printf.sprintf "pdflatex %s %s.tex" mode family_out in
  let error = Sys.command cmmd1 in
  if error <> 0 then (
    Printf.eprintf "Error in pdflatex processing (%d)\n" error;
    exit 0);
  if !test then exit 0;
  (* makeindex does not like absolute paths! *)
  let cmmd2 = Printf.sprintf "makeindex %s.idx" family_out in
  for _i = 0 to !index do
    let error = Sys.command cmmd2 in
    if error <> 0 then (
      Printf.eprintf "Error in makeindex processing (%d)\n" error;
      exit 0)
  done;
  let error = Sys.command cmmd1 in
  if error <> 0 then (
    Printf.eprintf "Error in 2nd pdflatex processing (%d)\n" error;
    exit 0);
  Printf.eprintf "Done all\n"

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
