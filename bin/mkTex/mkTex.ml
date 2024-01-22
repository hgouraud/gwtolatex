(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex
open Config

type name = string * string

(* TODO suppress (pages liées) and (modifier) in m=NOTES *)
(* TODO suppress "base chausey ..." *)

let new_tree = ref [ [] ]
let new_row = ref []
let c_width = ref 0
let c_span = ref 1
let c_typ = ref ""
let c_txt = ref ""
let c_item = ref ""
let c_img = ref ""

(* execution context *)
let passwd = ref ""
let out_file = ref ""
let dev = ref false
let second = ref false
let index = ref 0
let dict1 = ref (Hashtbl.create 100)
let dict2 = ref (Hashtbl.create 100)
let img_name_list = ref []

(* Assumes we are running in bases folder GeneWeb security constraint *)
let gw2l_dist = ref "./gw2l_dist"
let livres = ref "../livres"
let test = ref false
let follow = ref false
let test_nb = ref 0

(* current values *)
let treemode = ref 0
let passe = ref 0
let caption = ref ""

(* launch setup *)
let bases = ref ""
let base_name = ref "x"
let passwd = ref ""
let family = ref ""
let debug = ref 0
let verbose = ref false
let treemode = ref 1

(* chapter section images numbering *)
let chapter = ref 0
let section = ref 0
let subsection = ref 0
let subsubsection = ref 0
let subsubsubsection = ref 0
let current_level = ref 0
let image_nbr = ref 0
let images_in_page = ref []

(* defaults *)
let imgwidth_default = 5.1
let textwidth_default = 15.5
let textheight_default = 22.5
let rule_thickns_default = 0.5
let vignwidth_default = 1.5
let margin_default = 2.5
let colsep_default = 0.1

let make_conf xbases xbase_name xpasswd xfamily xdebug xverbose xtreemode =
  Printf.eprintf "******* Set conf : base: %s, family: %s\n" !base_name !family;
  let conf =
    {
      bases = xbases;
      base_name = xbase_name;
      passwd = xpasswd;
      family = xfamily;
      debug = xdebug;
      verbose = xverbose;
      treemode = xtreemode;
      (* formatting *)
      unit = "cm";
      textwidth = textwidth_default;
      textheight = textheight_default;
      margin = margin_default;
      colsep = colsep_default;
      rule_thickns = rule_thickns_default;
      fontsize = "";
      imgwidth = imgwidth_default;
      vignwidth = vignwidth_default;
      portraitwidth = imgwidth_default;
      sideways = false;
      twopages = false;
      double = false;
      split = 0;
      (* mkTex *)
      arbres = true;
      sub = false;
      collectimages = true;
      section_on_a_tag = true;
      highlights = [];
      hrule = true;
      imagelabels = 3;
      nbimgperline = 3;
      offset = false;
      wide = false;
      xoffset = 0.0;
      yoffset = 0.0;
    }
  in
  conf

type p_type = Str | Int | Bool

let print_conf conf =
  Printf.eprintf "Configuration\n";
  Printf.eprintf "  Launch parameters:\n";
  Printf.eprintf "    bases = %s\n" conf.bases;
  Printf.eprintf "    base_name = %s\n" conf.base_name;
  Printf.eprintf "    passwd = %s\n" conf.passwd;
  Printf.eprintf "    family = %s\n" conf.family;
  Printf.eprintf "    debug = %d\n" conf.debug;
  Printf.eprintf "    verbose = %s\n" (if conf.verbose then "true" else "false");
  Printf.eprintf "    treemode = %d\n" conf.treemode;
  Printf.eprintf "  Formatting:\n";
  Printf.eprintf "    unit = %s\n" conf.unit;
  Printf.eprintf "    textwidth = %1.2f\n" conf.textwidth;
  Printf.eprintf "    textheight = %1.2f\n" conf.textheight;
  Printf.eprintf "    margin = %1.2f\n" conf.margin;
  Printf.eprintf "    colsep = %1.2f\n" conf.colsep;
  Printf.eprintf "    rule_thickns = %1.2f\n" conf.rule_thickns;
  Printf.eprintf "    fontsize = %s\n" conf.fontsize;
  Printf.eprintf "    imgwidth = %1.2f\n" conf.imgwidth

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

let print_image conf (im_type, name, (ch, sec, ssec, sssec), nb) =
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
  | Portrait ->
      (* TODO manage images location *)
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s.%s}\n"
        conf.portraitwidth
        conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "images"; conf.base_name ])
        Filename.dir_sep
        (* GeneWeb replaces ' ' by '_' in key computations *)
        (Sutil.lower name |> Sutil.replace '-' '_' |> Sutil.replace ' ' '_')
        "jpg"
  | Imagek ->
      (* TODO manage images location *)
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s.%s}\n"
        conf.imgwidth conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "images"; conf.base_name ])
        Filename.dir_sep
        (* GeneWeb replaces ' ' by '_' in key computations *)
        (Sutil.lower name |> Sutil.replace '-' '_' |> Sutil.replace ' ' '_')
        "jpg"
  | Images ->
      let image_id =
        match List.assoc_opt name !img_name_list with
        | Some id -> id
        | None -> ""
      in
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s}%s\n"
        conf.imgwidth conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep
           [ "."; "src"; conf.base_name; "images" ])
        Filename.dir_sep name
        (if image_id <> "" && false then
         Format.sprintf "\\newcommand{ref_%s}{%d.%d.%d.%d}" image_id ch sec ssec
           nb
        else "")
      (* TODO deal with !caption here, possibly \begin{image}...\end{image} *)
  | Vignette ->
      Format.sprintf "\\includegraphics[width=%1.2f%s]{%s%s%s}\n" conf.vignwidth
        conf.unit
        (String.concat Filename.dir_sep
           [ "."; "src"; conf.base_name; "images" ])
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
  [ "button"; "head"; "form"; "select"; "colgroup"; "script"; "title"; "style" ]

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
   <href="http://127.0.0.1:2317/Chausey?m=D;p=albert;n=marie;v=2;image=off;t=T;dag=on;templ=tex">
   <end>

   <begin page>
   <sideways="1">
   <title="Arbre ascendant d'Eugénie Collet">
   <index="Collet, Eugénie (ep Vaillant)">
   <href="http://127.0.0.1:2317/Chausey?m=A;p=eugenie (x);n=collet;v=5;siblings=on;notes=on;t=T;after=;before=;dag=on;templ=tex">
   <end>
*)

let skip_m_cmd = [ "MOD_NOTES" ]
let one_page och line = output_string och line

(** process_tree_cumul accumulates results in a string
    each tag is processed according to its role
*)

let rec process_tree_cumul conf base och cumul tree (row, col) =
  let get_child children =
    List.fold_left
      (fun acc c -> acc ^ process_tree_cumul conf base och "" c (row, col))
      "" children
  in

  let continue cumul children =
    List.fold_left
      (fun acc c -> acc ^ process_tree_cumul conf base och cumul c (row, col))
      cumul children
  in

  let make_image_str name k content mode caption =
    let vignette = Sutil.contains name "-vignette" in
    if k = "" && (not vignette) && not (mode = "wide") then incr image_nbr;
    match mode with
    | "wide" ->
        let minipage_b = Format.sprintf "\\begin{figure}" in
        let minipage_e = Format.sprintf "\\end{figure}" in
        let caption =
          if caption <> "" then
            Format.sprintf "\\captionof{figure}{{\\it %s}}" caption
          else ""
        in
        Format.sprintf "%s%s\\includegraphics[width=%2.2f%s]{%s%s%s}%s%s"
          content minipage_b conf.textwidth
          conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
          (String.concat Filename.dir_sep
             [ "."; "src"; conf.base_name; "images" ])
          Filename.dir_sep name caption minipage_e
    | _ ->
        let image =
          ( Images,
            name,
            (!chapter, !section, !subsection, !subsubsection),
            !image_nbr )
        in
        if conf.collectimages && k = "" && not vignette then (
          images_in_page := image :: !images_in_page;
          match conf.imagelabels with
          | 1 ->
              Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d)}}" content
                !image_nbr
          | 3 ->
              Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d.%d)}}" content
                !chapter !section !image_nbr
          | 4 ->
              Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d.%d.%d)}}"
                content !chapter !section !subsection !image_nbr
          | _ ->
              Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d.%d)}}" content
                !chapter !section !image_nbr)
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
    let src_dir =
      String.concat Filename.dir_sep [ conf.bases; "src"; conf.base_name ]
    in
    let ic =
      try Some (open_in (Filename.concat src_dir (v ^ ".txt"))) with _ -> None
    in
    match ic with
    | None -> Format.sprintf " (Missing file %s.txt) " v
    | Some ic ->
        let file = really_input_string ic (in_channel_length ic) in
        if not (Sutil.contains file "<!-- mapped image -->") then
          Format.sprintf "{\\it %s}\\footnote{SRC or DOC file}" content
        else
          (* TODO use lower!! *)
          let i0 = Sutil.contains_index file "<img src=" in
          let i1 = Sutil.contains_index file "<img SRC=" in
          let i = match (i1, i1) with -1, -1 -> -1 | _, _ -> max i0 i1 in
          let j = String.index_from file i '>' in
          if j = -1 then
            Format.sprintf "Funny SRC content %s"
              (String.sub file i (min 40 (String.length file - i)))
          else
            let href = String.sub file i (j - i) in
            let href_attrl = Hutil.split_href href in
            let k = Hutil.get_href_attr "k" href_attrl in
            let s = Hutil.get_href_attr "s" href_attrl in
            if s <> "" then
              (* TODO see if wide/caption can be used here *)
              make_image_str s k content "" ""
              ^ "\\footnote{Image cliquable sur la version Internet}"
            else Format.sprintf "Funny SRC content %s" href
  in

  (*
  <a href="%sm=SRC;v=grande-ile-aerien">
  permet de localiser presque toutes les maisons.<br>
  Pour plus de détails, voir les plans cadastraux
  (<a href="%sm=SRC;v=plan-pointe-du-phare">Le Phare</a> et
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
    let attr = get_att_list attributes in
    let href = try List.assoc "href" attr with Not_found -> "" in
    let mode = try List.assoc "mode" attr with Not_found -> "" in
    let caption = try List.assoc "caption" attr with Not_found -> "" in
    let href_attrl = Hutil.split_href href in
    let b = Hutil.get_href_attr "b" href_attrl in
    let m = Hutil.get_href_attr "m" href_attrl in
    let i = Hutil.get_href_attr "i" href_attrl in
    let p = Hutil.get_href_attr "p" href_attrl in
    let n = Hutil.get_href_attr "n" href_attrl in
    let oc = Hutil.get_href_attr "oc" href_attrl in
    let k = Hutil.get_href_attr "k" href_attrl in
    let s = Hutil.get_href_attr "s" href_attrl in
    let v = Hutil.get_href_attr "v" href_attrl in
    let t = Hutil.get_href_attr "t" href_attrl in
    if List.mem m skip_m_cmd then ""
    else
      let content = get_child children in
      (* between <a...> and </a> *)
      let fn, sn, ocn, _sp, index_s =
        Hutil.get_real_person base i p n oc content
      in
      let test_hl =
        Format.sprintf "(%s, %s%s)" sn fn
          (if ocn <> 0 then Format.sprintf " (%d)" ocn else "")
      in
      let str =
        if m = "SRC" || m = "DOC" then read_src_file v content
        else if m = "D" && t = "V" then
          Format.sprintf "%s\\\\m=D\\&{}t=V\\\\ not available " content
        else if
          String.lowercase_ascii b <> String.lowercase_ascii conf.base_name
        then
          Format.sprintf "%s\\footnote{%s}" content
            (Sutil.replace '&' ';' href |> Sutil.decode |> Lutil.escape)
        else if s <> "" then make_image_str s k content mode caption
        else if m = "CAL" then content ^ index_s
        else if Sutil.contains content "includegraphics" then
          "{\\bf " ^ content ^ "}"
        else if List.mem test_hl conf.highlights then
          "{\\hl {\\bf " ^ content ^ "}}"
        else "{\\bf " ^ content ^ "}" ^ index_s
      in
      str
  in

  (* <img SRC="%sm=IM;s=grande-ile-aerien-v.jpg"></a> *)
  (* images called with <img = > are displayed "immediately" *)
  (* special treatment for vignettes *)
  let tag_img _name attributes _children =
    let attr = get_att_list attributes in
    let href = try List.assoc "src" attr with Not_found -> "" in
    let href_attrl = Hutil.split_href href in
    let p = Hutil.get_href_attr "p" href_attrl in
    let n = Hutil.get_href_attr "n" href_attrl in
    let oc = Hutil.get_href_attr "oc" href_attrl in
    let i = Hutil.get_href_attr "i" href_attrl in
    let k = Hutil.get_href_attr "k" href_attrl in
    let s = Hutil.get_href_attr "s" href_attrl in
    let str =
      let fn, sn, ocn, _sp, index_s = Hutil.get_real_person base i p n oc "" in
      (* TODO lower?? *)
      let ocn = if ocn = 0 then "0" else string_of_int ocn in
      let image_label = Format.sprintf "%s.%s.%s" fn ocn sn in
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
      print_image conf image ^ index_s
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
    | Text s -> if s = "\n" then " " else s
    | Element (name, attributes, children) (* as elt *) -> (
        match name with
        | ("i" | "u" | "b" | "em" | "tt" | "strong" | "tiny" | "small" | "big")
          as t ->
            let content = get_child children in
            Lutil.simple_tag_1 t content
        (* TODO check here we are terminating something!! *)
        | "br" -> "\\newline\n"
        | "sup" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\textsuperscript{%s}" content
            else ""
        | "font" ->
            let content = get_child children in
            content
        | "h1" ->
            let content = get_child children in
            if content <> "" && conf.section_on_a_tag then
              Format.sprintf "\\section{%s}" content
            else ""
        | "h2" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\subsection{%s}" content
            else ""
        | "h3" ->
            let content = get_child children in
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
            if content <> "" then str else "xxx"
        | "hr" -> "\\par\\noindent\\rule{\\textwidth}{0.4pt}\n"
        | "p" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\par\n %s" content else ""
        | "table" ->
            let content = get_child children in
            content
        | "caption" ->
            (* TODO clarify this <caption> with mode="caption" *)
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
            (* <span style="display:none" mode="tex">\index{Gelin, Zacharie}</span> *)
            (* or \highlight *)
            (* <span style="display:none" mode="highlight">sn fn%if;(oc != "0") (oc)%end;</span> *)
            (* or instruction for display next image *)
            (* <span style="display:none" mode="caption">Caption</span> applied to the next image *)
            (* children is a single string of TeX *)
            (* % might not be there (old format) *)
            let content = get_child children in
            let display_none =
              Hutil.test_attr attributes "style" "display:none"
            in
            let mode = Hutil.get_attr attributes "mode" in
            let old_tex_mode =
              (* for backward compatibility *)
              if String.length content > 3 then String.sub content 0 3 = "tex"
              else false
            in
            let mode =
              match mode with
              | "tex" -> "tex"
              | "highlight" -> "highlight"
              | "wide" -> "wide"
              | "a_ref" -> "a_ref"
              | "" -> if old_tex_mode then "tex" else ""
              | _ -> ""
            in
            let str =
              if display_none then
                match mode with
                | "tex" ->
                    (* extract TeX command from content *)
                    if String.length content > 3 then
                      let content = Sutil.replace '[' '{' content in
                      let content = Sutil.replace ']' '}' content in
                      (* TODO in includegraphics replace {width=xxyy} by [width=xxyy] *)
                      let reg1 = Str.regexp {|{width=\(.*\)}|} in
                      let content =
                        if Str.string_match reg1 content 0 then
                          let xxyy = Str.matched_group 1 content in
                          let new_s = "[width=" ^ xxyy ^ "]" in
                          Str.global_replace reg1 new_s content
                        else content
                      in
                      let content =
                        if old_tex_mode then
                          String.sub content 4 (String.length content - 7)
                        else content
                      in
                      let i =
                        (* ???? old format !! *)
                        try String.index_from content 0 '%'
                        with Not_found -> -1
                      in
                      if i > 0 then
                        String.sub content 0 i
                        ^ String.sub content (i + 1)
                            (String.length content - i - 1)
                      else content
                    else content
                | "highlight" ->
                    (* highlight content *)
                    if List.mem content conf.highlights then
                      Format.sprintf "{\\hl{\\bf %s}}" content
                    else content
                | "caption" ->
                    caption := content;
                    ""
                | _ -> ""
                (* <span mode="a_ref" gw2w="which" gw2sn="snxx"
                     gw2fn="fnxx" gw2oc="ocxx" gw2al="alxx">content</span> *)
                (* this mode allows the template code to pass parameters to mkTex *)
              else if mode = "a_ref" then
                let hl, str =
                  let _w = Hutil.get_attr attributes "gw2w" in
                  let sn = Hutil.get_attr attributes "gw2sn" in
                  let fn = Hutil.get_attr attributes "gw2fn" in
                  let oc = Hutil.get_attr attributes "gw2oc" in
                  let _al = Hutil.get_attr attributes "gw2al" in
                  let fn, sn, oc, _sp, index_l =
                    Hutil.get_real_person base "" fn sn oc ""
                  in
                  let test_hl =
                    Format.sprintf "%s, %s%s" sn fn
                      (if oc <> 0 then Format.sprintf " (%d)" oc else "")
                  in
                  (List.mem test_hl conf.highlights, index_l)
                in
                (if hl then Format.sprintf "{\\hl " ^ content ^ "}"
                else content)
                ^ str
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
                (fun acc c ->
                  acc ^ process_tree_cumul conf base och cumul c tabl)
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
            if !new_row <> [] then new_tree := !new_row :: !new_tree;
            Trees.print_tree conf (List.rev !new_tree)
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
            c_typ := "Vr1";
            continue "" children
        | "emptycell" ->
            c_typ := "E";
            continue "" children
        | "newline" ->
            new_row :=
              (!c_width, !c_span, !c_typ, !c_txt, !c_item, !c_img) :: !new_row;
            new_tree := !new_row :: !new_tree;
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

let process_html conf base och body =
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
    | Some tree -> process_tree_cumul conf base och "" tree (0, 0)
    | _ -> "bad tree"
  in
  output_string och content

let bad_code c = c >= 400

(* <x Cmd param>remain *)
(*       i     j       *)
let one_command conf och line =
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
  | "Arbres" | "Trees" ->
      if param = "on" || param = "On" then
        { conf with imgwidth = 1.5; arbres = true }
      else { conf with imgwidth = imgwidth_default; arbres = false }
  | "BumpSub" -> { conf with sub = param = "on" || param = "On" }
  | "Chapter" ->
      out "chapter" param;
      incr chapter;
      image_nbr := if conf.imagelabels > 1 then 0 else conf.imagelabels;
      current_level := 0;
      section := 0;
      subsection := 0;
      subsubsection := 0;
      conf
  | "CollectImages" ->
      { conf with collectimages = param = "on" || param = "On" }
  | "Fiches" -> { conf with section_on_a_tag = param = "on" || param = "On" }
  | "FontSize" ->
      {
        conf with
        fontsize =
          (if param = "off" || param = "Off" || param = "" then "" else param);
      }
  | "HighLight" ->
      {
        conf with
        highlights =
          (if param = "off" || param = "Off" then []
          else param :: conf.highlights);
      }
  | "Hrule" -> { conf with hrule = param = "on" || param = "On" }
  | "ImageLabels" ->
      {
        conf with
        imagelabels = (try int_of_string param with Failure _ -> 3);
      }
  | "ImgWidth" -> { conf with imgwidth = Float.of_string param }
  | "NbImgPerLine" ->
      let nb = try int_of_string param with Failure _ -> 3 in
      {
        conf with
        nbimgperline = nb;
        imgwidth = conf.textwidth /. Float.of_int nb;
      }
  | "Input" ->
      (let param = Sutil.replace_str param "%%%LIVRES%%%" !livres in
       let param = Sutil.replace_str param "%%%GW2L_DIST%%%" !gw2l_dist in
       let param = Sutil.replace_str param "%%%PASSWD%%%" !passwd in
       let ic = open_in param in
       try
         while true do
           let line = input_line ic in
           let line = Sutil.replace_str line "%%%LIVRES%%%" !livres in
           let line = Sutil.replace_str line "%%%GW2L_DIST%%%" !gw2l_dist in
           let line = Sutil.replace_str line "%%%PASSWD%%%" !passwd in
           output_string och (line ^ "\n")
         done
       with End_of_file -> close_in ic);
      conf
  | "LaTeX" ->
      output_string och param;
      conf
  | "Newpage" ->
      output_string och "\\newpage";
      conf
  | "Print" ->
      output_string och param;
      conf
  | "Sideways" -> { conf with sideways = param = "on" || param = "On" }
  | "TwoPages" -> { conf with twopages = param = "on" || param = "On" }
  | "Split" -> { conf with split = int_of_string param }
  | "DoubleCells" -> { conf with double = param = "on" || param = "On" }
  | "Section" ->
      out "section" param;
      incr section;
      image_nbr := if conf.imagelabels > 2 then 0 else !image_nbr;
      current_level := 1;
      subsection := 0;
      subsubsection := 0;
      conf
  | "SubSection" ->
      out "subsection" param;
      incr subsection;
      image_nbr := if conf.imagelabels > 3 then 0 else !image_nbr;
      current_level := 2;
      subsubsection := 0;
      conf
  | "SubSubSection" ->
      out "subsubsection" param;
      incr subsubsection;
      image_nbr := if conf.imagelabels > 4 then 0 else !image_nbr;
      current_level := 3;
      conf
  | "Version" ->
      output_string och (Sutil.version ^ "\n");
      conf
  | "WideImages" ->
      {
        conf with
        imgwidth =
          (if param = "on" || param = "On" then conf.textwidth
          else imgwidth_default);
        wide = param = "on" || param = "On";
      }
  | "TextWidth" -> { conf with textwidth = Float.of_string param }
  | "TextHeight" -> { conf with textheight = Float.of_string param }
  | "VignWidth" ->
      {
        conf with
        vignwidth =
          (if param = "off" || param = "Off" then 1.0
          else Float.of_string param);
      }
  | "TreeMode" -> { conf with treemode = int_of_string param }
  | "Xoffset" -> { conf with offset = true; xoffset = Float.of_string param }
  | "Yoffset" -> { conf with offset = true; yoffset = Float.of_string param }
  | "Offset" ->
      {
        conf with
        offset = param = "on" || param = "On";
        xoffset = 0.0;
        yoffset = 0.0;
      }
  | "Unit" -> { conf with unit = param }
  | "Debug" -> { conf with debug = int_of_string param }
  | _ ->
      output_string och (Format.sprintf "%%%s%s\n" cmd remain);
      conf

let one_http_call conf base och line =
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
              (Format.sprintf "Bad code when fetching %s: %d!\n"
                 (Lutil.escape url) code))
          else
            let _ = process_html conf base och body in
            ()
      | Error (_, msg) ->
          Printf.eprintf "error when fetching %s\n %s\n%!" url
            (Lutil.escape msg);
          output_string och
            (Format.sprintf "Error when fetching %s:\n %s\n" (Lutil.escape url)
               (Lutil.escape msg))

(** print all images mentioned in the notes of a person *)
let print_images conf och images_list =
  output_string och (Format.sprintf "\\par\n");
  (* TODO manage Wide *)
  List.iter
    (fun (im_type, name, (ch, sec, ssec, _sssec), nbr) ->
      (* let wide = Sutil.contains name "-wide" in *)
      let width = Format.sprintf "%2.2f%s" conf.imgwidth conf.unit in
      match im_type with
      | Imagek | Portrait | Vignette -> ()
      | Images ->
          let name1 = Sutil.replace_str name "\\_{}" "_" in
          let name = Filename.remove_extension name in
          let images_dir =
            String.concat Filename.dir_sep
              [ "."; "src"; conf.base_name; "images" ]
          in
          let image_id =
            match List.assoc_opt name1 !img_name_list with
            | Some id -> id
            | None -> ""
          in
          let img_number =
            match conf.imagelabels with
            | 1 -> Format.sprintf "\n\\hglabxsa{%d}{%d}{%d}" ch sec nbr
            | 3 -> Format.sprintf "\n\\hglabxa{%d}{%d}{%d}" ch sec nbr
            | 4 -> Format.sprintf "\n\\hglabxb{%d}{%d}{%d}{%d}" ch sec ssec nbr
            | _ -> Format.sprintf "\n\\hglabxa{%d}{%d}{%d}" ch sec nbr
          in
          let img_label =
            if image_id <> "" then Format.sprintf "\\label{img_ref_%s}" image_id
            else ""
          in
          (* list of persons present on this image *)
          (* TODO les personnes /z ont été éliminées!! *)
          let index_list =
            match Hashtbl.find_opt !dict1 image_id with
            | Some (anx_page, _desc, _fname, key_l) when image_id <> "" ->
                let index_l =
                  List.fold_left
                    (fun acc (key : MkImgDict.key) ->
                      let sn = Sutil.replace '_' ' ' key.pk_surname in
                      let sn = Sutil.particles sn in
                      let fn = Sutil.replace '_' ' ' key.pk_first_name in
                      Format.sprintf "\\index{%s, %s%s, photo %s}" sn fn
                        (if key.pk_occ <> 0 then
                         Format.sprintf " (%d)" key.pk_occ
                        else "")
                        (if anx_page <> "0" then
                         Format.sprintf "%s/%s" image_id anx_page
                        else image_id)
                      :: acc)
                    [] key_l
                in
                String.concat "" index_l
            | _ -> ""
          in
          output_string och
            (Format.sprintf
               "\\parbox{%s}{\\includegraphics[width=%s]{%s%s%s}\\newline%s%s%s}\n"
               width width images_dir Filename.dir_sep
               (Sutil.replace_str name "\\_{}" "_")
               img_number img_label index_list))
    (List.rev images_list);
  output_string och (Format.sprintf "\\par\n")

let process_one_line conf base _dict1 _dict2 och line =
  (*Printf.eprintf "process_one_line: %s, base: %s\n" line conf.base_name;*)
  let line = Sutil.replace_str line "%%%LIVRES%%%" !livres in
  let line = Sutil.replace_str line "%%%BASE%%%" conf.base_name in
  let line = Sutil.replace_str line "%%%PASSWD%%%" conf.passwd in
  let conf =
    match line.[0] with
    | '<' -> (
        match line.[1] with
        | 'a' ->
            let content = get_a_content line in
            let href = Hutil.get_href line in
            let href_attrl = Hutil.split_href href in
            let i = Hutil.get_href_attr "i" href_attrl in
            let p = Hutil.get_href_attr "p" href_attrl in
            let n = Hutil.get_href_attr "n" href_attrl in
            let oc = Hutil.get_href_attr "oc" href_attrl in
            (* get_real_person builds \index if any *)
            let _fn, _sn, _ocn, _sp, _index_s =
              Hutil.get_real_person base i p n oc content
            in
            let sec =
              (* -> chapter, 1-> section, ... *)
              match !current_level with
              | 0 ->
                  incr section;
                  ""
              | 1 ->
                  if conf.sub then (
                    incr subsection;
                    "sub")
                  else (
                    incr section;
                    "")
              | 2 ->
                  if conf.sub then (
                    incr subsubsection;
                    "subsub")
                  else (
                    incr subsection;
                    "sub")
              | _ ->
                  if conf.sub then "subsubsub"
                  else (
                    incr subsubsubsection;
                    "subsub")
            in
            image_nbr :=
              if conf.imagelabels > !current_level + 1 then 0 else !image_nbr;
            let index =
              if Sutil.contains line "\\index" then "" (* index manually done *)
              else Format.sprintf "\\index{%s}" content (* automatic index *)
            in
            output_string och
              (Format.sprintf "\\%ssection{%s%s}\n" sec content index);

            image_nbr := if conf.imagelabels = 4 then 0 else !image_nbr;

            one_http_call conf base och line;

            Printf.eprintf "Collect images: %s\n"
              (if conf.collectimages then "yes" else "no");

            if conf.collectimages && !images_in_page <> [] then
              print_images conf och !images_in_page;
            images_in_page := [];
            if conf.hrule then
              output_string och (Format.sprintf "\\par\\vspace{0.5cm}\\hrule\n");
            conf
        | 'x' -> one_command conf och line
        | 'y' ->
            output_string och "";
            conf
        | _ ->
            output_string och (line ^ "\n");
            conf)
    | _ ->
        output_string och (line ^ "\n");
        conf
  in
  conf

(* current version reads family.txt and runs pdflatex and makeindex *)
(* in a short future makeBook will handle the whole process including  *)
(* addition in each personnal page of image index information *)

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> base_name := x), " Choose base.");
      ("-passwd", Arg.String (fun x -> passwd := x), " Set wizard password.");
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
      ("-treemode", Arg.Int (fun x -> treemode := x), " Print tree mode.");
      ( "-pass",
        Arg.Int (fun x -> passe := x),
        " Pass one or two (collect img references)." );
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
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;

  Printf.eprintf "******* Arg parse : base: %s, family: %s\n" !base_name !family;
  let conf =
    make_conf !bases !base_name !passwd !family !debug !verbose !treemode
  in

  Printf.eprintf "******* After make_conf : base: %s, family: %s\n"
    conf.base_name conf.family;

  print_conf conf;

  let img_file =
    String.concat Filename.dir_sep
      [ !livres; !family ^ "-inputs"; "who_is_where.txt" ]
  in

  (* build images dictionnaries *)
  if !verbose then Printf.printf "Build images dicts\n";
  let dict1_t, dict2_t, img_name_l = MkImgDict.create_images_dicts img_file in
  dict1 := dict1_t;
  dict2 := dict2_t;
  img_name_list := img_name_l;

  let fname_txt, _family_out =
    ( (if !family <> "" then Filename.concat !livres (!family ^ ".txt")
      else Printf.sprintf "test/gwtolatex-test%d.txt" !test_nb),
      if !family <> "" then !family
      else Printf.sprintf "gwtolatex-test%d" !test_nb )
  in
  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_all = Filename.concat !livres (!family ^ ".txt") in
  let fname_out = !out_file in
  let mode, fname_in =
    if Sys.file_exists fname_txt then ("txt", fname_txt)
    else if Sys.file_exists fname_htm then ("html", fname_htm)
    else ("", fname_all)
  in

  (* TODO find a way to open base remotely *)
  let base = Hutil.open_base (Filename.concat "." !base_name) in

  let och = open_out fname_out in
  let ic = open_in_bin fname_in in
  if !debug = -1 then Sys.enable_runtime_warnings false;

  Printf.eprintf
    "This is \027[32mmkTeX\027[0m version %s for %s on base %s to %s (%d)\n"
    Sutil.version conf.family conf.base_name fname_out conf.debug;
  flush stderr;

  let tmp = ref conf in

  (match mode with
  | "html" ->
      (* for testing purposes *)
      let body = really_input_string ic (in_channel_length ic) in
      let _ = process_html conf base och body in
      close_in ic;
      close_out och;
      exit 0
  | _ -> (
      try
        while true do
          tmp := conf;
          let line = input_line ic in
          tmp := process_one_line !tmp base dict1 dict2 och line
        done
      with End_of_file ->
        close_in ic;
        close_out och));
  Printf.eprintf "\n";
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
