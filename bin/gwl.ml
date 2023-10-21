(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex

type name = string * string

(* TODO suppress (pages liées) and (modifier) in m=NOTES *)
(* TODO suppress "base chausey ..." *)
type my_tree =
  | Text of string
  | Element of string * (name * string) list * my_tree list

(* TODO Imagek = Portrait !! *)
type im_type = Portrait | Imagek | Images | Vignette

type _image = {
  im_type : im_type;
  filename : string;
  where : int * int * int * int; (* ch, sec, ssec, sssec*)
  image_nbr : int;
}

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
let c_img = ref ""

(* execution context *)
let base = ref "chausey"
let family = ref ""
let out_file = ref ""
let debug = ref (-1)
let mode = ref 0
let dev = ref false
let second = ref false
let index = ref 0
let verbose = ref false

(* Assumes we are running in bases folder GeneWeb security constraint *)
let livres = ref (try Sys.getenv "GWTL_LIVRES" with Not_found -> "./Livres")
let bases = ref (try Sys.getenv "GWTL_BASES" with Not_found -> "./")
let test = ref false
let follow = ref false
let test_nb = ref 0
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
let sub = ref false
let _trees = ref false
let ep = ref false
let arbres = ref false
let sideways = ref false
let textwidth = ref 15.5 (* in cm *)
let textheight = ref 22.5
let margin = ref 2.5
let fontsize = ref ""
let hrule = ref true
let imgwidth_default = 5.1
let imgwidth = ref imgwidth_default
let vignwidth = ref 1.0
let unit = ref "cm"
let offset = ref false
let xoffset = ref 0.0
let yoffset = ref 0.0
let twopages = ref false

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

let _strip_all_trailing_spaces s =
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

let chop_body n body = String.sub body 0 (min n (String.length body))

(* returns content between matching tags, and following body *)
let _find_matching_tag name body =
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
      else if String.length body > j + 1 && body.[j + 1] = '/' then
        let found =
          let rec loop k =
            if k = String.length name then true
            else if body.[j + 2 + k] = name.[k] then loop (k + 1)
            else false
          in
          loop 0
        in
        if found then
          (* <tag>content</tag>, body *)
          let content = String.sub body 0 j in
          let body =
            String.sub body
              (j + 3 + String.length name)
              (String.length body - (j + 3 + String.length name))
          in
          let body =
            if body <> "" && body.[0] = '\n' then
              String.sub body 1 (String.length body - 1)
            else body
          in
          (content, body)
        else match_tag (j + 1) body
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

let _dump_tag elt =
  match elt with
  | Element (name, attributes, children) ->
      Printf.eprintf "<begin %s>\n" name;
      List.iter
        (fun ((_a, b), c) -> Printf.eprintf "  attr: %s=%s\n" b c)
        attributes;
      dump children 0;
      Printf.eprintf "<end %s>\n" name
  | Text s -> Printf.eprintf "Text elt: (%s)\n" s

(* TODO find TaTeX equivalent string *)
(*
    & % $ # _ { } ~ ^ \

Outside \verb, the first seven of them can be typeset 
by prepending a backslash; for the other three,
use the macros \textasciitilde, \textasciicircum, and \textbackslash.
*)

let get_att_list attributes =
  List.fold_left (fun acc ((_, k), v) -> (k, v) :: acc) [] attributes

let print_image (im_type, name, (ch, sec, ssec, sssec), nb) =
  let _trace =
    Format.sprintf "Type: %s, name: %s, (%d, %d, %d, %d), nb: %d"
      (match im_type with
      | Portrait -> "Portrait"
      | Images -> "Images"
      | Imagek -> "Imagek"
      | Vignette -> "Vignette")
      name ch sec ssec sssec nb
  in
  match im_type with
  | Portrait | Imagek ->
      (* TODO manage images location *)
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s.%s}\n" !imgwidth
        !unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "images"; !base ])
        Filename.dir_sep
        (* GeneWeb replaces ' ' by '_' in key computations *)
        (Sutil.lower name |> Sutil.replace '-' '_' |> Sutil.replace ' ' '_')
        "jpg"
  | Images ->
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s}\n" !imgwidth
        !unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "src"; !base; "images" ])
        Filename.dir_sep name
  | Vignette ->
      Format.sprintf "\\includegraphics[width=%1.2f%s]{%s%s%s}\n" !vignwidth
        !unit
        (String.concat Filename.dir_sep [ "."; "src"; !base; "images" ])
        Filename.dir_sep name

(* ignore tag but read children *)
let dummy_tags_0 = [ "body"; "html"; "center"; "bdo"; "table" ]

(* ignore tag, skip to end *)
let dummy_tags_1 = [ "!--"; "samp" ]

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
    "wide";
  ]

let dummy_tags_3 =
  [
    "button";
    "head";
    "form";
    "select";
    "colgroup";
    "script";
    "title";
    "style";
  ]

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

  let get_child children =
    List.fold_left
      (fun acc c -> acc ^ process_tree_cumul och "" c (row, col))
      "" children
  in

  let continue cumul children =
    List.fold_left
      (fun acc c -> acc ^ process_tree_cumul och cumul c (row, col))
      cumul children
  in

  let make_image_str s k content =
    let vignette = Sutil.contains s "-vignette" in
    if k = "" && not vignette then incr image_nbr;
    let image =
      ( Images,
        s,
        (!chapter, !section, !subsection, !subsubsection),
        !image_nbr )
    in
    if !collect_images && k = "" && not vignette then (
      images_in_page := image :: !images_in_page;
      match !image_label with
      | 1 ->
          Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d)}}" content
            !image_nbr
      | 3 ->
          Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d.%d)}}"
            content !chapter !section !image_nbr
      | 4 ->
          Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d.%d.%d)}}"
            content !chapter !section !subsection !image_nbr
      | _ ->
          Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d.%d)}}"
            content !chapter !section !image_nbr)
    else content
  in

  (* SRC file typically contain mapped images *)
  (* for more complex SRC files, see later!! *)
  (*
  <!-- mapped image -->
  <h1>La Grande Île</h1>
  <img SRC="%sm=IM;s=grande-ile-aerien.jpg" border="0" usemap="#Grande-Ile-Aerien">
  <map name="Grande-Ile-Aerien">
  *)

  let read_src_file v content =
    let src_dir = String.concat Filename.dir_sep [ !bases; "src"; !base ] in
    let ic =
      try Some (open_in (Filename.concat src_dir (v ^ ".txt")))
      with _ -> None
    in
    match ic with
    | None -> Format.sprintf " (Missing file %s.txt) " v
    | Some ic ->
        let file = really_input_string ic (in_channel_length ic) in
        if not (Sutil.contains file "<!-- mapped image -->") then
            Format.sprintf "{\\it %s}\\footnote{SRC or DOC file}" content
        else
          let i0 = Sutil.contains_index file "<img src=" in
          let i1 = Sutil.contains_index file "<img SRC=" in
          let i = match i1, i1 with -1, -1 -> -1 | _, _ -> max i0 i1 in
          let j = String.index_from file i '>' in
          if j = -1 then
            Format.sprintf "Funny SRC content %s"
              (String.sub file i (min 40 ((String.length file) - i)))
          else
            let href = String.sub file i (j - i) in
            let href1 = Sutil.decode href in
            let href_attrl = Hutil.split_href href1 in
            let k = Hutil.get_href_attr "k" href_attrl in
            let s = Hutil.get_href_attr "s" href_attrl in
            if s <> "" then (make_image_str s k content) ^
                "\\footnote{Image cliquable sur la version Internet}"
            else
              Format.sprintf "Funny SRC content %s" href
  in
  (*
  <a href="%sm=SRC;v=grande-ile-aerien">
  <img SRC="%sm=IM;s=grande-ile-aerien-v.jpg"></a>
  permet de localiser presque toutes les maisons.<br>
  Pour plus de détails, voir les plans cadastraux (<a href="%sm=SRC;v=plan-pointe-du-phare">Le Phare</a> et
  <a href="%sm=SRC;v=plan-blainvillais">Blainvillais</a>) ou la
  <a href="%sm=SRC;v=grande-ile">carte de l’île</a>.
  *)

  let tag_a _name attributes children =
    (* if p <> "" or n <> "" we have a person *)
    (* it may appear undes a different spelling as part of <a>xxx</a> *)
    (* or we may have a portrait k <> "" *)
    (* or an image m=IM|IMH|DOC|SRC *)
    (* <a href=m=IM...k=p.oc.n><img src=m=IM...k=p.oc.n></a> *)
    (* assumes that children is a single string *)
    (* which may be an <img src=xxx> *)
    (* TODO m=TT t=xxx p=yyy *)
    (* TODO decode names  p=louis;n=de%20bourbon; *)
    (* TODO identify vignettes ! -> special width *)
    let attr = get_att_list attributes in
    let href = try List.assoc "href" attr with Not_found -> "" in
    let href = Sutil.decode href in
    let href_attrl = Hutil.split_href href in
    let b = Hutil.get_href_attr "b" href_attrl in
    let m = Hutil.get_href_attr "m" href_attrl in
    let p = Hutil.get_href_attr "p" href_attrl in
    let n = Hutil.get_href_attr "n" href_attrl in
    let oc = Hutil.get_href_attr "oc" href_attrl in
    let i = Hutil.get_href_attr "i" href_attrl in
    let k = Hutil.get_href_attr "k" href_attrl in
    let s = Hutil.get_href_attr "s" href_attrl in
    let v = Hutil.get_href_attr "v" href_attrl in
    let t = Hutil.get_href_attr "t" href_attrl in
    if List.mem m skip_m_cmd then ""
    else (
      let content = get_child children in
      (* between <a...> and </a> *)
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
        if (fn <> "" || sn <> "") && s <> "" && k = "" then
          Lutil.build_index fn sn ocn content check
        else if m = "SRC" || m = "DOC" then read_src_file v content
        else if m = "D" && t = "V" then
          Format.sprintf "%s\\\\m=D\\&{}t=V\\\\ not available " content
        else if String.lowercase_ascii b <> String.lowercase_ascii !base then
          Format.sprintf "%s\\footnote{%s}" content (Lutil.escape href)
        else if s <> "" then make_image_str s k content
        else ("{\\bf " ^ content ^ "}")
      in
      str)
  in

  let tag_img _name attributes _children =
    let attr = get_att_list attributes in
    let href = try List.assoc "src" attr with Not_found -> "" in
    let href = Sutil.decode href in
    let href_attrl = Hutil.split_href href in
    let p = Hutil.get_href_attr "p" href_attrl in
    let n = Hutil.get_href_attr "n" href_attrl in
    let oc = Hutil.get_href_attr "oc" href_attrl in
    let i = Hutil.get_href_attr "i" href_attrl in
    let k = Hutil.get_href_attr "k" href_attrl in
    let s = Hutil.get_href_attr "s" href_attrl in
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
      (* TODO identify vignettes ! -> special width *)
      let vignette = Sutil.contains s "-vignette" in
      if not vignette then incr image_nbr;
      let image =
        ( (if vignette then Vignette
          else if k <> "" then Imagek
          else if s <> "" then Images
          else Portrait),
          (if k <> "" then image_label else s),
          (!chapter, !section, !subsection, !subsubsection),
          !image_nbr )
      in
      if !collect_images && not vignette then
        images_in_page := image :: !images_in_page;
      print_image image
    in
    str
  in

  let _print_cell cell =
    match cell with
    | w, s, ty, te, it ->
        Printf.eprintf "(%d, %d, %s, %s, %s) " w s ty
          (Sutil.clean_double_back_slash te)
          (Sutil.clean_double_back_slash it)
  in

  let element =
    match tree with
    | Text s -> s
    | Element (name, attributes, children) (* as elt *) -> (
        match name with
        | ("i" | "u" | "b" | "em" | "tt" | "strong" | "tiny" | "small" | "big")
          as t ->
            let content = get_child children in
            Lutil.simple_tag_1 t content
        (* TODO check here we are terminating something!! *)
        | "br" -> " \\\\\n"
        | "sup" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\textsuperscript{%s}" content
            else ""
        | "font" -> 
            let content = get_child children in
            content
        | "h1" ->
            let content = get_child children in
            if content <> "" && !section_on_a_tag then
              Format.sprintf "\\section{%s}" content
            else ""
        | "h2" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\subsection{%s}" content
            else ""
        | "h3" -> (
            let content = get_child children in
            Printf.eprintf "Doing h3 tag: %s\n" content;
            (* TODO parametrer ce comportement *)
            (* TODO revoir comportement côté LaTeX *)
            (* TODO this is language dependant !! *)
            (* généraliser aux autres <hn>  </hn> *)
            let str = 
              if Sutil.contains content "Bateaux" then
                "\n\\par\\hgbato{Bateaux}"
              else if Sutil.contains content "Occupants" then
                "\n\\par\\hgbato{Occupants}"
              else Format.sprintf "\\subsubsection{%s}" content
            in
            if content <> "" then str else "xxx")
        | "hr" -> "\\par\\noindent\\rule{\\textwidth}{0.4pt}\n"
        | "p" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\par\n %s" content else ""
        | "table" ->
            let content = get_child children in
            content
        | "caption" ->
            let content = get_child children in
            let content = Format.sprintf "\n\\caption{%s}\n" content in
            content
        | "tbody" ->
            (* implicit if no <caption> or <thead> *)
            (* TODO what happens if <caption> ?? *)
            let content = get_child children in
            (* TODO compute the number of columns and their style *)
            (* Ignore <tables> for the time being *)
            content
        | "tr" ->
            let content = get_child children in
            content
        | "td" ->
            let content = get_child children in
            content
        | "ul" ->
            let content = get_child children in
            if content <> "" then
              Format.sprintf "\\begin{hgitemize}\n %s\n\\end{hgitemize}\n"
                content
            else ""
        | "li" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\item{}{%s}" content else ""
        | "span" ->
            (* look for potential TeX code *)
            (* <span style="display:none">tex \index%{Gelin, Zacharie}tex</span> *)
            (* or \highlight *)
            (* <span mode="highlight">sn fn%if;(oc != "0") (oc)%end;</span> *)
            (* children is a single string of TeX *)
            (* % might not be there (old format) *)
            let display_none =
              Hutil.test_attr attributes "style" "display:none"
            in
            let highlight_mode =
              Hutil.test_attr attributes "mode" "highlight"
            in
            let tex_mode_1 = Hutil.test_attr attributes "mode" "tex" in
            let str =
              let content = get_child children in
              let tex_mode_2 =
                if String.length content > 3 then String.sub content 0 3 = "tex"
                else false
              in
              if display_none then
                if String.length content > 3 && (tex_mode_1 || tex_mode_2) then
                  let content = Sutil.replace '[' '{' content in
                  let content = Sutil.replace ']' '}' content in
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
                  else content
                else content
              else if highlight_mode then
                if List.mem content !highlights then
                  Format.sprintf "{\\hl{\\bf %s}}" content
                else Format.sprintf "{\\bf %s}" content
              else content
            in
            str
        | "div" ->
            (* <div class="container" style="column-count:2;column-gap:50px"> *)
            (* <div class="column"> *)
            (* <div class="row"> *)
            let clas = Hutil.get_attr attributes "class" in
            let sty = Hutil.get_attr attributes "style" in
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
            let tabl =
              if Sutil.contains clas "columns" then (0, 0) else (row, col)
            in
            let content =
              List.fold_left
                (fun acc c -> acc ^ process_tree_cumul och cumul c tabl)
                "" children
            in
            if Sutil.contains clas "container" && cols > 1 then
              Format.sprintf "\n\\begin{multicols}{%d}\n%s\n\\end{multicols}\n"
                cols content
            else content
        (* Trees ********* NON REENTRANT !! ********** *)
        (* <tables> in trees dont work !!              *)
        | "bigtree" ->
            new_tree := [];
            new_row := [];
            let _ = continue "" children in
            if !new_row <> [] then new_tree := List.rev !new_row :: !new_tree;
            Trees.print_tree !base (List.rev !new_tree) !mode !textwidth
              !textheight !margin !debug !fontsize !sideways !imgwidth !twopages
        | "cell" ->
            if !c_typ <> "" || !c_txt <> "" || !c_item <> "" || !c_img <> ""
            then
              new_row :=
                (!c_width, !c_span, !c_typ, !c_txt, !c_item, !c_img) :: !new_row;
            c_width := 0;
            let span = Hutil.get_attr attributes "colspan" in
            (c_span := try int_of_string span with Failure _ -> 1);
            (* TODO undestand why some span are -1 *)
            if !c_span = -1 then c_span := 1;
            c_typ := "";
            c_txt := "";
            c_item := "";
            c_img := "";
            continue "" children
        | "celltext" ->
            c_typ := "Te";
            c_txt := Lutil.escape (get_child children);
            continue "" children
        | "cellitem" ->
            c_typ := "It";
            c_item := Lutil.escape (get_child children);
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
            c_typ := "Hv1";
            continue "" children
        | "emptycell" ->
            c_typ := "E";
            continue "" children
        | "newline" ->
            new_row :=
              (!c_width, !c_span, !c_typ, !c_txt, !c_item, !c_img) :: !new_row;
            new_tree := List.rev !new_row :: !new_tree;
            new_row := [];
            c_typ := "";
            c_txt := "";
            c_item := "";
            c_img := "";
            continue "" children
        | "image" ->
            c_typ := "Im";
            c_img := Lutil.escape (get_child children);
            continue "" children
        (* end trees ***************************** *)
        | name when List.mem name dummy_tags_0 -> continue cumul children
        | name when List.mem name dummy_tags_1 -> ""
        | name when List.mem name dummy_tags_2 -> ""
        | name when List.mem name dummy_tags_3 -> ""
        (* cumul is handled by these two tag functions *)
        | "a" -> tag_a name attributes children
        | "img" -> tag_img name attributes children
        | _ ->
            Printf.eprintf "Missing tag: %s\n" name;
            "")
  in
  cumul ^ element

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
  | "Arbres" ->
      if param = "on" || param = "On" then (
        imgwidth := 1.5;
        arbres := true)
      else (
        imgwidth := imgwidth_default;
        arbres := false)
  | "BumpSub" -> sub := param = "on" || param = "On"
  | "Chapter" ->
      if !image_label > 1 then image_nbr := 0;
      out "chapter" param;
      incr chapter;
      current_level := 0;
      section := 0;
      subsection := 0;
      subsubsection := 0
  | "CollectImages" -> collect_images := param = "on" || param = "On"
  | "Ep" -> ep := param = "on" || param = "On"
  | "Fiches" -> section_on_a_tag := param = "on" || param = "On"
  | "FontSize" -> fontsize := param
  | "HighLight" -> highlights := param :: !highlights
  | "Hrule" -> hrule := param = "on" || param = "On"
  | "ImageLabels" -> (
      image_label := try int_of_string param with Failure _ -> 3)
  | "ImageWidth" -> imgwidth := Float.of_string param
  | "Input" -> (
      let param = Sutil.replace_str param "%%%LIVRES%%%" !livres in
      let ic = open_in param in
      try
        while true do
          let line = input_line ic in
          let line = Sutil.replace_str line "%%%LIVRES%%%" !livres in
          output_string och (line ^ "\n")
        done
      with End_of_file -> close_in ic)
  | "LaTeX" -> output_string och param
  | "Newpage" -> output_string och "\\newpage"
  | "Print" -> output_string och param
  | "Sideways" -> sideways := param = "on" || param = "On"
  | "TwoPages" -> twopages := param = "on" || param = "On"
  | "Section" ->
      if !image_label > 2 then image_nbr := 0;
      out "section" param;
      incr section;
      current_level := 1;
      subsection := 0;
      subsubsection := 0
  | "SubSection" ->
      if !image_label > 3 then image_nbr := 0;
      out "subsection" param;
      incr subsection;
      current_level := 2;
      subsubsection := 0
  | "SubSubSection" ->
      if !image_label > 4 then image_nbr := 0;
      out "subsubsection" param;
      incr subsubsection;
      current_level := 3
  | "Trees" ->
      if param = "on" || param = "On" then (
        imgwidth := 1.5;
        arbres := true)
      else (
        imgwidth := imgwidth_default;
        arbres := false)
  | "Version" -> output_string och (version ^ "\n")
  | "WideImage" ->
      if param = "on" || param = "On" then (
        imgwidth := !textwidth;
        wide := true)
      else (
        imgwidth := imgwidth_default;
        wide := false)
  | "TextWidth" -> textwidth := Float.of_string param
  | "VignWidth" ->
      if param = "off" || param = "Off" then vignwidth := 1.0
      else vignwidth :=  Float.of_string param
  | "TreeMode" -> mode := int_of_string param
  | "Xoffset" ->
      offset := true;
      xoffset := Float.of_string param
  | "Yoffset" ->
      offset := true;
      yoffset := Float.of_string param
  | "Offset" ->
      if param = "on" || param = "On" then offset := true
      else (
        offset := false;
        xoffset := 0.0;
        yoffset := 0.0)
  | "Unit" -> unit := param
  | _ -> output_string och (Format.sprintf "%%%s%s\n" cmd remain)

let one_http_call och line =
  let url_beg = try String.index_from line 0 '"' with Not_found -> 0 in
  let url_end =
    try String.index_from line (url_beg + 1) '"' with Not_found -> 0
  in
  let url = String.sub line (url_beg + 1) (url_end - url_beg - 1) in
  let parts = String.split_on_char '?' line in
  if List.length parts > 1 then
    let evars =
      String.split_on_char '&' (Sutil.replace ';' '&' (List.nth parts 1))
    in
    let evars =
      List.map
        (fun kv ->
          let parts = String.split_on_char '=' kv in
          if List.length parts > 1 then (List.nth parts 0, List.nth parts 1)
          else (List.nth parts 0, ""))
        evars
    in
    if
      try List.assoc "m" evars = "D" && List.assoc "t" evars = "V"
      with Not_found -> false
    then output_string och "Command m=D;t=V is not available (yet)\\\\\n"
    else
      let resp = Ezcurl.get ~url () in
      match resp with
      | Ok { Ezcurl.code; body; _ } ->
          if bad_code code then (
            Printf.eprintf "bad code when fetching %s: %d\n%!" url code;
            output_string och
              (Format.sprintf "Bad code when fetching %s: %d!\n" url code))
          else
            let _ = process_html och body in
            ()
      | Error (_, msg) ->
          Printf.eprintf "error when fetching %s:\n  %s\n%!" (Lutil.escape url)
            (Lutil.escape msg);
          output_string och
            (Format.sprintf "Error when fetching %s:\n %s\n" (Lutil.escape url)
               (Lutil.escape msg))

let print_images och images_list =
  output_string och (Format.sprintf "\\par\n");
  (* TODO manage Wide *)
  List.iter
    (fun (im_type, name, (ch, sec, ssec, _sssec), nbr) ->
      let wide = Sutil.contains name "-wide" in
      let width =
        if wide then "\\textwidth" else Printf.sprintf "%2.2f%s" !imgwidth !unit
      in
      match im_type with
      | Imagek | Portrait | Vignette -> ()
      | Images ->
          let name = Filename.remove_extension name in
          let images_dir =
            "/Users/Henri/Genea/GeneWeb-Bases/src/chausey/images"
          in
          let label =
            match !image_label with
            | 1 -> Format.sprintf "\n\\hglabxsa{%d}{%d}{%d}" ch sec nbr
            | 3 -> Format.sprintf "\n\\hglabxa{%d}{%d}{%d}" ch sec nbr
            | 4 ->
                Format.sprintf "\n\\hglabxb{%d}{%d}{%d}{%d}" ch sec ssec nbr
            | _ -> Format.sprintf "\n\\hglabxa{%d}{%d}{%d}" ch sec nbr
          in
          output_string och
            (Format.sprintf
               "\\parbox{%s}{\\includegraphics[width=%s]{%s%s%s}\\\\%s}\n" width
               width images_dir Filename.dir_sep
               (Sutil.replace_str name "\\_{}" "_")
               label))
    (List.rev images_list);
  output_string och (Format.sprintf "\\par\n")

let process_one_line och line =
  let line = Sutil.replace_str line "%%%LIVRES%%%" !livres in
  match line.[0] with
  | '<' -> (
      match line.[1] with
      | 'a' ->
          let content = get_a_content line in
          let sec =
            (* -> chapter, 1-> section, ... *)
            match !current_level with
            | 0 ->
                incr section;
                ""
            | 1 ->
                if !sub then (
                  incr subsection;
                  "sub")
                else (
                  incr section;
                  "")
            | 2 ->
                if !sub then (
                  incr subsubsection;
                  "subsub")
                else (
                  incr subsection;
                  "sub")
            | _ ->
                if !sub then "subsubsub"
                else (
                  incr subsubsection;
                  "subsub")
          in
          if !image_label > !current_level + 1 then image_nbr := 0;
          let index =
            if String.contains line '\\' then "" (* index manually done *)
            else Format.sprintf "\\index{%s}" content (* automatic index *)
          in
          output_string och
            (Format.sprintf "\\%ssection{%s%s}\n" sec content index);
          (match !image_label with 1 -> () | 4 -> image_nbr := 0 | _ -> ());
          one_http_call och line;
          if !collect_images && !images_in_page <> [] then
            print_images och !images_in_page;
          images_in_page := [];
          if !hrule then output_string och (Format.sprintf "\\par\\hrule\n")
      | 'b' -> one_page och line
      | 'x' -> one_command och line
      | 'y' -> output_string och ""
      | _ -> output_string och (line ^ "\n"))
  | _ -> output_string och (line ^ "\n")

let show_process_time start =
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
      ( "-index",
        Arg.Int (fun x -> index := x),
        " Number of times makeindex is done." );
      ("-second", Arg.Set second, " Run Pdflatex a second time.");
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-mode", Arg.Int (fun x -> mode := x), " Print tree mode.");
      ("-v", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
      ("-follow", Arg.Set follow, " Run Pdflatex.");
      ("-pdf", Arg.Set follow, " Run Pdflatex.");
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

  (* install tex templates in bases/etc *)
  (* on excute dans le repo (dev) ou dans bases_dir *)
  let dist_dir = if !dev then "." else "./gw2l_dist" in
  let etc_dir = Filename.concat !bases "etc" in
  let tex_dir = String.concat Filename.dir_sep [ dist_dir; "tex" ] in
  let do_load_tex_files = Format.sprintf "cp -R %s %s" tex_dir etc_dir in
  let error = Sys.command do_load_tex_files in
  if error > 0 then (
    Printf.eprintf "Error while loading tex templates files (%d)\n" error;
    exit 0);

  (*
  let tmp_dir = Filename.concat dist_dir "tmp" in
  try if Sys.is_directory tmp_dir then ()
  with Sys_error _ -> (
    Printf.eprintf "Creating tmp dir\n";
    try Sys.mkdir tmp_dir 766 with Sys_error _ -> (
      Printf.eprintf "Error in creating tmp dir\n"; exit 0));
*)
  let fname_txt, family_out =
    ( (if !family <> "" then Filename.concat !livres (!family ^ ".txt")
      else Printf.sprintf "test/gwtolatex-test%d.txt" !test_nb),
      if !family <> "" then !family
      else Printf.sprintf "gwtolatex-test%d" !test_nb )
  in
  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_all = Filename.concat !livres (!family ^ ".txt") in
  let fname_out =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; family_out ^ ".tex" ]
  in
  let mode, fname_in =
    if Sys.file_exists fname_txt then ("txt", fname_txt)
    else if Sys.file_exists fname_htm then ("html", fname_htm)
    else ("", fname_all)
  in
  let och = if !follow then open_out fname_out else stderr in
  let ic = open_in_bin fname_in in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  Printf.eprintf "\nThis is GwToLaTeX version %s on %s to %s (%d)\n" version
    fname_in fname_out !debug;
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
  Printf.eprintf "Done txt parsing in %s s\n" (show_process_time start_time);
  flush stderr;

  let mode = if !verbose then "" else "-interaction=batchmode" in
  let fname =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; family_out ^ ".aux" ]
  in
  let do_rm_aux = Printf.sprintf "rm %s" fname in
  let do_pdflatex =
    Printf.sprintf "pdflatex -output-directory=%s %s %s"
      (String.concat Filename.dir_sep [ dist_dir; "tmp" ])
      mode fname_out
  in

  Printf.eprintf "First pass at pdflatex \n";
  flush stderr;
  let _ = Sys.command do_rm_aux in
  let error = Sys.command do_pdflatex in
  if error <> 0 then (
    Printf.eprintf "Error in pdflatex processing (%d)\n" error;
    Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
    exit 0);

  Printf.eprintf "Building index\n";
  flush stderr;
  if !test && !index = 0 then (
    Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
    exit 0);
  (* makeindex does not like absolute paths! *)
  let fname =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; family_out ^ ".idx" ]
  in
  let do_makeindex = Printf.sprintf "makeindex %s" fname in
  for _i = 0 to !index do
    let error = Sys.command do_makeindex in
    if error <> 0 then (
      Printf.eprintf "Error in makeindex processing (%d)\n" error;
      Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
      exit 0)
  done;

  if !second then (
    Printf.eprintf "Second pass at pdflatex \n";
    flush stderr;
    let error = Sys.command do_pdflatex in
    if error <> 0 then
      Printf.eprintf "Error in 2nd pdflatex processing (%d)\n" error;
    let error = Sys.command do_makeindex in
    if error <> 0 then
      Printf.eprintf "Error in makeindex processing (%d)\n" error);

  let fname = Filename.basename fname_out |> Filename.remove_extension in
  let pdf_name =
    String.concat Filename.dir_sep [ dist_dir; "tmp"; fname ^ ".pdf" ]
  in
  let dir = if !dev then "test" else !livres in
  let do_move_pdf = Printf.sprintf "mv %s %s" pdf_name dir in
  let error = Sys.command do_move_pdf in
  if error <> 0 then Printf.eprintf "Error in moving pdf file (%d)\n" error;

  Printf.eprintf "Result file is in %s\n" (Filename.concat dir (fname ^ ".pdf"));
  Printf.eprintf "Process time is %s s\n" (show_process_time start_time);
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
