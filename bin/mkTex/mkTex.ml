(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex
open Config
module Driver = Geneweb_db.Driver

type _name = string * string

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
let gw_dir = ref (try Sys.getenv "GW_BIN" with Not_found -> "./")
let _passwd = ref ""
let out_file = ref ""
let dev = ref false
let dry_run = ref false
let gwtest = ref false
let second = ref false
let index = ref 0
let dict1 = ref (Hashtbl.create 100)
let dict2 = ref (Hashtbl.create 100)
let img_name_list = ref []
let img_ok_list = ref []
let missing_tags = ref []

(* Assumes we are running in bases folder GeneWeb security constraint *)
let gw2l_dist = ref "./gw2l_dist"
let livres = ref "../livres"
let test = ref false
let follow = ref false
let test_nb = ref 0

(* current values *)
let _treemode = ref 0
let passe = ref 0
let caption = ref ""

(* launch setup *)
let bases = ref ""
let basename = ref "x"
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
let current_level = ref 0
let image_nbr = ref 0
let images_in_page = ref []

(* defaults *)
let imgwidth_default = 5.1
let portraitwidth_default = 5.1
let textwidth_default = 15.5
let textheight_default = 24.5
let rulethickns_default = 0.5
let vignwidth_default = 1.5
let margin_default = 2.5
let colsep_default = 0.05

let make_conf xbases xbasename xpasswd xfamily xdebug xverbose xtreemode =
  let conf =
    {
      bases = xbases;
      basename = xbasename;
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
      rulethickns = rulethickns_default;
      fontsize = "";
      imgwidth = imgwidth_default;
      vignwidth = vignwidth_default;
      portraitwidth = imgwidth_default;
      sideways = false;
      twopages = false;
      samepage = false;
      double = false;
      expand = 0;
      split = 0;
      (* mkTex *)
      arbres = true;
      collectimages = true;
      sectiononatag = true;
      highlights = [];
      hrule = true;
      imagelabels = 3;
      nbimgperline = 3;
      offset = false;
      wide = false;
      hoffset = 0.0;
      voffset = 0.0;
    }
  in
  conf

type _p_type = Str | Int | Bool

let string_conf conf =
  let config_str =
    [
      Format.sprintf "Configuration\n";
      Format.sprintf "  Launch parameters:\n";
      Format.sprintf "    bases = %s\n" conf.bases;
      Format.sprintf "    basename = %s\n" conf.basename;
      Format.sprintf "    passwd = %s\n" conf.passwd;
      Format.sprintf "    family = %s\n" conf.family;
      Format.sprintf "    debug = %d\n" conf.debug;
      Format.sprintf "    verbose = %s\n"
        (if conf.verbose then "true" else "false");
      Format.sprintf "    treemode = %d\n" conf.treemode;
      Format.sprintf "  Formatting:\n";
      Format.sprintf "    unit = %s\n" conf.unit;
      Format.sprintf "    textwidth = %1.2f\n" conf.textwidth;
      Format.sprintf "    textheight = %1.2f\n" conf.textheight;
      Format.sprintf "    margin = %1.2f\n" conf.margin;
      Format.sprintf "    colsep = %1.2f\n" conf.colsep;
      Format.sprintf "    rulethickns = %1.2f (pt)\n" conf.rulethickns;
      Format.sprintf "    fontsize = %s\n" conf.fontsize;
      Format.sprintf "    imgwidth = %1.2f\n" conf.imgwidth;
      Format.sprintf "    vignwidth = %1.2f\n" conf.vignwidth;
      Format.sprintf "    portraitwidth = %1.2f\n" conf.portraitwidth;
      Format.sprintf "  Other params :\n";
      Format.sprintf "    collectimages = %s\n"
        (if conf.collectimages then "true" else "false");
      Format.sprintf "    sectiononatag = %s\n"
        (if conf.sectiononatag then "true" else "false");
      Format.sprintf "    nbimgperline = %d\n" conf.nbimgperline;
      Format.sprintf "    sideways = %s\n"
        (if conf.sideways then "true" else "false");
      Format.sprintf "    twopages = %s\n"
        (if conf.twopages then "true" else "false");
      Format.sprintf "    samepage = %s\n"
        (if conf.samepage then "true" else "false");
      Format.sprintf "    double = %s\n"
        (if conf.double then "true" else "false");
      Format.sprintf "    arbres =  %s\n"
        (if conf.arbres then "true" else "false");
      Format.sprintf "    split =  %d\n" conf.split;
    ]
  in
  String.concat "" config_str

let dump_conf conf =
  let config_str = string_conf conf in
  Printf.eprintf "%s" config_str

let print_conf conf =
  let config_str = string_conf conf in
  Format.sprintf "\\begin{verbatim}\n%s\\end{verbatim}" config_str

let strip_tr_sp s =
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
      | Vignette -> "Vignette"
      | Wide -> "Wide")
      name ch sec ssec sssec nb
  in
  let wide = Sutil.contains name "-wide" in
  match im_type with
  | Portrait ->
      (* TODO manage images location *)
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s.%s}\n"
        conf.portraitwidth
        conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "images"; conf.basename ])
        Filename.dir_sep
        (* GeneWeb replaces ' ' by '_' in key computations *)
        (Sutil.lower name |> Sutil.replace '-' '_' |> Sutil.replace ' ' '_')
        "jpg"
  | Imagek ->
      (* TODO manage images location *)
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s.%s}\n"
        conf.imgwidth conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "images"; conf.basename ])
        Filename.dir_sep
        (* GeneWeb replaces ' ' by '_' in key computations *)
        (Sutil.lower name |> Sutil.replace '-' '_' |> Sutil.replace ' ' '_')
        "jpg"
  | Images | Wide ->
      let image_id =
        match List.assoc_opt name !img_name_list with
        | Some id -> id
        | None -> 0
      in
      let _anx_page, _desc, _fname, _key_l, _key_l_2, _image_occ =
        match Hashtbl.find_opt !dict1 image_id with
        | Some (anx_page, desc, fname, key_l, key_l_2, image_occ) ->
            (anx_page, desc, fname, key_l, key_l_2, image_occ)
        | None ->
            Printf.eprintf "Print image: %d non existant (2)!\n" image_id;
            (0, "dummy", "", [], [], [])
      in
      img_ok_list := image_id :: !img_ok_list;
      Format.sprintf "\n\\includegraphics[width=%1.2f%s]{%s%s%s}\n"
        (if wide then conf.textwidth else conf.imgwidth)
        conf.unit (* 5 cm in page mode, 1.5 cm in table mode *)
        (String.concat Filename.dir_sep [ "."; "src"; conf.basename; "images" ])
        Filename.dir_sep name
      (* TODO deal with !caption here, possibly \begin{image}...\end{image} *)
  | Vignette ->
      Format.sprintf "\\includegraphics[width=%1.2f%s]{%s%s%s}\n" conf.vignwidth
        conf.unit
        (String.concat Filename.dir_sep [ "."; "src"; conf.basename; "images" ])
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
(* obsolete *)

let skip_m_cmd = [ "MOD_NOTES" ]
let _one_page och line = output_string och line

(** process_tree_cumul accumulates results in a string each tag is processed
    according to its role *)

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
        (* TODO rework. Do not use figure. Has its own numbering scheme! *)
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
             [ "."; "src"; conf.basename; "images" ])
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
          | 2 ->
              Format.sprintf "%s{\\raisebox{.6ex}{\\small (%d.%d)}}" content
                !chapter !image_nbr
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
      String.concat Filename.dir_sep [ "."; "src"; conf.basename ]
    in
    let ic =
      try Some (open_in (Filename.concat src_dir (v ^ ".txt"))) with _ -> None
    in
    match ic with
    | None ->
        Format.sprintf " (Missing file %s) "
          (Filename.concat src_dir (v ^ ".txt"))
    | Some ic ->
        let file = really_input_string ic (in_channel_length ic) in
        if not (Sutil.contains file "usemap=") then
          Format.sprintf
            "{\\it %s}\\footnote{Fichier SRC ou DOC. tbd plus-tard}" content
        else
          (* TODO use lower!! *)
          let i0 = Sutil.contains_index file "<img src=" in
          let i1 = Sutil.contains_index file "<img SRC=" in
          let i = match (i0, i1) with -1, -1 -> -1 | _, _ -> max i0 i1 in
          let j = if i >= 0 then String.index_from file i '>' else -1 in
          if i = -1 || j = -1 || i + 9 >= String.length file || j - i - 9 < 0
          then
            Format.sprintf "Funny SRC content %s"
              (Filename.concat src_dir (v ^ ".txt"))
          else
            let href = String.sub file (i + 9) (j - i - 9) in
            let href_attrl = Hutil.split_href href in
            let k = Hutil.get_href_attr "k" href_attrl in
            let s = Hutil.get_href_attr "s" href_attrl in
            if s <> "" then
              (* TODO see if wide/caption can be used here *)
              make_image_str s k content "" ""
              ^ "\\footnote{Image (peut être) cliquable sur la version \
                 Internet}"
            else Format.sprintf "Funny SRC content %s" href
  in

  (*
  <a href="%sm=SRC;v=grande-ile-aerien">
  permet de localiser presque toutes les maisons.<br>
  Pour plus de détails, voir les plans cadastraux
  (<a href="%sm=SRC;v=plan-pointe-du-phare">Le Phare</a> et
  <a href="%sm=SRC;v=plan-blainvillais">Blainvillais</a>) ou la
  <a href="%sm=SRC;v=grande-ile">carte de l’île</a>.
  <a mode="wide" caption="Légende de l'image"
     href="%sm=SRC;v=grande-ile">carte de l’île</a>.
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
        else if String.lowercase_ascii b <> String.lowercase_ascii conf.basename
        then
          Format.sprintf "%s\\footnote{%s}" content
            (Sutil.replace '&' ';' href |> Sutil.decode |> Lutil.escape)
        else if s <> "" then make_image_str s k content mode caption
        else if m = "CAL" then content ^ index_s
        else if Sutil.contains content "includegraphics" then
          "{\\bf " ^ content ^ "}"
        else if List.mem test_hl conf.highlights then
          "{\\hl {\\bf " ^ content ^ " xxx}}"
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
      let vignette = Sutil.contains s "-vignette" || Sutil.contains s "-v." in
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
        | "br" -> "\\par\n"
        | "sup" ->
            let content = get_child children in
            if content <> "" then Format.sprintf "\\textsuperscript{%s}" content
            else ""
        | "font" ->
            let content = get_child children in
            content
        | "h1" ->
            let content = get_child children in
            if content <> "" then
              Format.sprintf "\n\\par{\\Large %s}\\par\n" content
            else ""
        | "h2" ->
            let content = get_child children in
            if content <> "" then
              Format.sprintf "\n\\par{\\large %s}\\par\n" content
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
              else if content <> "" then
                Format.sprintf "\n\\par\\textbf{%s}\\par\n" content
              else ""
            in
            if content <> "" then str else ""
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
        | "th" ->
            let content = get_child children in
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
                    (* attention, \\textbf{{\\hl xxxx}} ne fonctionne pas *)
                    if List.mem content conf.highlights then
                      Format.sprintf "\\bf {{\\hl %s}}" content
                    else content
                | "caption" ->
                    caption := content;
                    ""
                | _ -> ""
              else if mode = "a_ref" then
                (* <span mode="a_ref" gw2w="which" gw2sn="snxx"
                     gw2fn="fnxx" gw2oc="ocxx" gw2al="alxx">content</span> *)
                (* this mode allows the template code to pass parameters to mkTex *)
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
                (if hl then Format.sprintf "{\\hl " ^ content ^ "}" else content)
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
            if not (List.mem name !missing_tags) then (
              Printf.eprintf "Missing tag: %s\n" name;
              missing_tags := name :: !missing_tags);
            "")
  in
  cumul ^ element

let guillemets_latex content =
  Str.global_replace (Str.regexp {|« |}) "«\\," content
  |> Str.global_replace (Str.regexp {| »|}) "\\,»"

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
  let content = guillemets_latex content in
  output_string och content

let bad_code c = c >= 400

let mark_section och =
  output_string och
    (Format.sprintf "\nsectionmark %d.%d.%d\n" !chapter !section !subsection)

(* <x Cmd param>remain *)
(*       i     j       *)
let one_command conf och line =
  let get_float_value line param default =
    let on_off = param = "off" || param = "Off" in
    match on_off with
    | true -> (true, default)
    | false ->
        ( false,
          try Float.of_string param
          with Failure _ ->
            Printf.eprintf "Bad param %s\n" line;
            default )
  in
  let get_int_value line param default =
    let on_off = param = "off" || param = "Off" in
    match on_off with
    | true -> (true, default)
    | false ->
        ( false,
          try int_of_string param
          with Failure _ ->
            Printf.eprintf "Bad param %s\n" line;
            default )
  in

  let len = String.length line in
  let line, len =
    if line.[len - 1] = '\n' then (String.sub line 0 (len - 1), len - 1)
    else (line, len)
  in
  let i = try String.index_from line 3 ' ' with Not_found -> -1 in
  let i =
    if i = -1 then try String.index_from line 3 '>' with Not_found -> -1
    else i
  in
  let j = try String.index_from line 0 '>' with Not_found -> -1 in
  let cmd = if i > 0 then String.sub line 3 (i - 3) else "" in
  let param =
    if i > 0 && i < len - 2 && j > 0 && j < len then
      String.sub line (i + 1) (j - i - 1)
    else "" |> String.trim
  in
  if cmd = "" then Printf.eprintf "Bad command: %s\n" line;
  let remain =
    if j > 0 && j < len - 1 then String.sub line (j + 1) (len - j - 1) else ""
  in
  let out c param =
    output_string och (Format.sprintf "\\%s{%s}%s\n" c param remain)
  in
  if conf.debug = 2 then
    Printf.eprintf "Cmd: %s, param: %s, remain: %s\n" cmd param remain;
  match cmd with
  | "Arbres" | "Trees" ->
      let arbres = param = "on" || param = "On" in
      if arbres then
        {
          conf with
          imgwidth = (if arbres then 1.5 else imgwidth_default);
          portraitwidth = (if arbres then 1.5 else portraitwidth_default);
          arbres;
        }
      else
        {
          conf with
          imgwidth = imgwidth_default;
          portraitwidth = portraitwidth_default;
          arbres;
        }
  | "BaseVersion" ->
      let bname =
        if String.sub !basename (String.length !basename - 4) 4 = "-new" then
          String.sub !basename 0 (String.length !basename - 4)
        else !basename
      in
      let bdir = Filename.concat !bases (bname ^ ".gwb") in
      (* Fonction pour convertir le temps en un format lisible *)
      let format_time time =
        let tm = Unix.localtime time in
        Printf.sprintf "%04d-%02d-%02d à %02dh%02d" (tm.tm_year + 1900)
          (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min
      in
      (try
         let stats = Unix.stat bdir in
         let access_time = stats.st_atime in
         output_string och
           (Format.sprintf
              "Le dernier accès à la base \\textbf{%s} a eu lieu le: %s" bname
              (format_time access_time))
       with Unix.Unix_error (err, _, _) ->
         output_string och
           (Format.sprintf "Erreur: %s\n" (Unix.error_message err)));
      conf
  (* TODO recode using make_conf *)
  | "Chapter" ->
      out "chapter" param;
      incr chapter;
      image_nbr := if conf.imagelabels > 1 then 0 else conf.imagelabels;
      current_level := 1;
      section := 0;
      subsection := 0;
      subsubsection := 0;
      mark_section och;
      conf
  | "CollectImages" ->
      { conf with collectimages = param = "on" || param = "On" }
  | "ColSep" ->
      let _off, value = get_float_value line param colsep_default in
      { conf with colsep = value }
  | "Debug" ->
      let _off, value = get_int_value line param 0 in
      { conf with debug = value }
  | "Defaults" ->
      {
        conf with
        treemode = 1;
        (* formatting *)
        unit = "cm";
        textwidth = textwidth_default;
        textheight = textheight_default;
        margin = margin_default;
        colsep = colsep_default;
        rulethickns = rulethickns_default;
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
        collectimages = true;
        sectiononatag = true;
        highlights = [];
        hrule = true;
        imagelabels = 3;
        nbimgperline = 3;
        offset = false;
        wide = false;
        hoffset = 0.0;
        voffset = 0.0;
      }
  | "DoubleCells" -> { conf with double = param = "on" || param = "On" }
  | "DumpConfig" ->
      dump_conf conf;
      conf
  | "Expand" ->
      let _off, value = get_int_value line param 0 in
      { conf with expand = value }
  | "FontSize" ->
      {
        conf with
        fontsize =
          (if param = "off" || param = "Off" || param = "" then "" else param);
      }
  | "GeneWebCommit" ->
      let branch = Gwversion.branch in
      let src = Gwversion.src in
      let commit_id = Gwversion.commit_id in
      let commit_date = Gwversion.commit_date in
      let compil_date = Gwversion.compil_date in
      output_string och
        (Format.sprintf
           {|
\textbf{GeneWeb} data:
\begin{itemize}
\item branch: %s
\item src: %s
\item commit id: %s
\item commit date: %s
\item compil date: %s
\end{itemize}
|}
           branch src commit_id commit_date compil_date);
      conf
  | "HighLight" ->
      {
        conf with
        highlights =
          (if param = "off" || param = "Off" then []
           else param :: conf.highlights);
      }
  | "Hrule" -> { conf with hrule = param = "on" || param = "On" }
  | "ImageLabels" ->
      let _off, value = get_int_value line param 3 in
      { conf with imagelabels = value }
  | "ImgWidth" ->
      let _off, value = get_float_value line param imgwidth_default in
      { conf with imgwidth = value }
  | "Incr" -> (
      let param = String.trim param in
      match param with
      | "Chapter" ->
          incr chapter;
          section := 0;
          subsection := 0;
          subsubsection := 0;
          conf
      | "Section" ->
          incr section;
          subsection := 0;
          subsubsection := 0;
          conf
      | "SubSection" ->
          incr subsection;
          subsubsection := 0;
          conf
      | "SubSubSection" ->
          incr subsubsection;
          conf
      | _ ->
          Printf.eprintf "Bad param: %s\n" line;
          conf)
  | "Input" ->
      (let param = Sutil.replace_str "%%%LIVRES%%%" !livres param in
       let param = Sutil.replace_str "%%%GW2L_DIST%%%" !gw2l_dist param in
       let param = Sutil.replace_str "%%%PASSWD%%%" !passwd param in
       let ic = open_in param in
       try
         while true do
           let line = input_line ic |> Sutil.strip_nl in
           let line = Sutil.replace_str "%%%LIVRES%%%" !livres line in
           let line = Sutil.replace_str "%%%GW2L_DIST%%%" !gw2l_dist line in
           let line = Sutil.replace_str "%%%PASSWD%%%" !passwd line in
           output_string och (strip_tr_sp line ^ "\n")
         done
       with End_of_file -> close_in ic);
      conf
  | "LaTeX" ->
      output_string och param;
      conf
  | "NbImgPerLine" ->
      let _off, value = get_int_value line param 3 in
      {
        conf with
        nbimgperline = value;
        imgwidth = conf.textwidth /. Float.of_int value;
      }
  | "NewPage" ->
      output_string och "\\newpage";
      conf
  | "Offset" ->
      let off, value = get_float_value line param 0.0 in
      { conf with offset = off; hoffset = value; voffset = 0.0 }
  | "PortraitWidth" ->
      let _off, value = get_float_value line param imgwidth_default in
      { conf with portraitwidth = value }
  | "Print" ->
      output_string och param;
      conf
  | "PrintConfig" ->
      output_string och (print_conf conf);
      conf
  | "Reset" -> (
      let param = String.trim param in
      match param with
      | "Chapter" ->
          chapter := 0;
          section := 0;
          subsection := 0;
          subsubsection := 0;
          conf
      | "Section" ->
          section := 0;
          subsection := 0;
          subsubsection := 0;
          conf
      | "SubSection" ->
          subsection := 0;
          subsubsection := 0;
          conf
      | "SubSubSection" ->
          subsubsection := 0;
          conf
      | _ -> conf)
  | "SamePage" -> { conf with samepage = param = "on" || param = "On" }
  | "Section" ->
      out "section" param;
      incr section;
      image_nbr := if conf.imagelabels > 2 then 0 else !image_nbr;
      current_level := 2;
      subsection := 0;
      subsubsection := 0;
      mark_section och;
      conf
  | "Sideways" ->
      {
        conf with
        sideways = param = "on" || param = "On";
        textwidth = textwidth_default;
        textheight = textheight_default;
      }
  | "Split" ->
      let _off, value = get_int_value line param 0 in
      { conf with split = value }
  | "SubSection" ->
      out "subsection" param;
      incr subsection;
      image_nbr := if conf.imagelabels > 3 then 0 else !image_nbr;
      current_level := 3;
      subsubsection := 0;
      mark_section och;
      conf
  | "SubSubSection" ->
      out "subsubsection" param;
      incr subsubsection;
      image_nbr := if conf.imagelabels > 4 then 0 else !image_nbr;
      current_level := 4;
      mark_section och;
      conf
  | "TextHeight" ->
      let _off, value = get_float_value line param textheight_default in
      { conf with textheight = value }
  | "TextWidth" ->
      let _off, value = get_float_value line param textwidth_default in
      { conf with textwidth = value }
  | "TreeMode" ->
      let _off, value = get_int_value line param 0 in
      { conf with treemode = value }
  | "TwoPages" -> { conf with twopages = param = "on" || param = "On" }
  | "Unit" -> { conf with unit = param }
  | "Version" ->
      output_string och (Sutil.version ^ "\n");
      conf
  | "VignWidth" ->
      let _off, value = get_float_value line param vignwidth_default in
      { conf with vignwidth = value }
  | "WideImages" ->
      {
        conf with
        imgwidth =
          (if param = "on" || param = "On" then conf.textwidth
           else imgwidth_default);
        wide = param = "on" || param = "On";
      }
  | "Hoffset" ->
      let off, value = get_float_value line param vignwidth_default in
      { conf with offset = off; hoffset = value }
  | "Voffset" ->
      let off, value = get_float_value line param vignwidth_default in
      { conf with offset = off; voffset = value }
  | _ ->
      output_string och (Format.sprintf "%s%s ???\n" cmd remain);
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
            if not !gwtest then
              output_string och
                (Format.sprintf "Bad code when fetching %s: %d!\n"
                   (Lutil.escape url) code))
          else if not (!dry_run || !gwtest) then process_html conf base och body
          else ()
      | Error (_, msg) ->
          Printf.eprintf "error when fetching %s\n %s\n%!" url
            (Lutil.escape msg);
          output_string och
            (Format.sprintf "Error when fetching %s:\n %s\n" (Lutil.escape url)
               (Lutil.escape msg))

(** print all images mentioned in the notes of a person *)
let print_images conf och images_list _key_str =
  output_string och (Format.sprintf "\\par\n");
  (* TODO manage Wide *)
  List.iter
    (fun (im_type, name, (ch, sec, ssec, _sssec), nbr) ->
      let width =
        Format.sprintf "%2.2f%s"
          (if Sutil.contains name "-wide" then conf.textwidth else conf.imgwidth)
          conf.unit
      in
      if Sutil.contains name "-wide" then Printf.eprintf "Wide image: %s\n" name;
      match im_type with
      | Imagek | Portrait | Vignette -> ()
      | Images | Wide ->
          let name1 = Sutil.replace_str "\\_{}" "_" name in
          let name = Filename.remove_extension name in
          let images_dir =
            String.concat Filename.dir_sep
              [ "."; "src"; conf.basename; "images" ]
          in
          let image_id =
            match List.assoc_opt name1 !img_name_list with
            | Some id -> id
            | None -> 0
          in
          let anx_page, desc, fname, key_l, key_l_2, image_occ =
            match Hashtbl.find_opt !dict1 image_id with
            | Some (anx_page, desc, fname, key_l, key_l_2, image_occ) ->
                (anx_page, desc, fname, key_l, key_l_2, image_occ)
            | None ->
                Printf.eprintf "Image_id (%d) (%s) non existant!\n" image_id
                  name1;
                (0, "dummy", "", [], [], [])
          in
          let img_number0 =
            match conf.imagelabels with
            | 1 -> Format.sprintf "%d.%d.%d" ch sec nbr
            | 3 -> Format.sprintf "%d.%d.%d" ch sec nbr
            | 4 -> Format.sprintf "%d.%d.%d.%d" ch sec ssec nbr
            | _ -> Format.sprintf "%d.%d.%d.%d" ch sec ssec nbr
          in
          Hashtbl.replace !dict1 image_id
            (anx_page, desc, fname, key_l, key_l_2, img_number0 :: image_occ);
          (*if image_id = 0 then Printf.eprintf "Name1: (%s)\n" name1;*)
          let img_number =
            match conf.imagelabels with
            | 1 -> Format.sprintf "\n\\hglabxsa{%d}{%d}{%d}" ch sec nbr
            | 3 -> Format.sprintf "\n\\hglabxa{%d}{%d}{%d}" ch sec nbr
            | 4 -> Format.sprintf "\n\\hglabxb{%d}{%d}{%d}{%d}" ch sec ssec nbr
            | _ -> Format.sprintf "\n\\hglabxa{%d}{%d}{%d}{%d}" ch sec ssec nbr
          in
          let img_label =
            if image_id <> 0 then
              Format.sprintf "\\label{img_ref_%d.%s}" image_id img_number0
            else ""
          in
          (* list of persons present on this image *)
          (* TODO les personnes /z ont été éliminées!! *)
          let index_list =
            match Hashtbl.find_opt !dict1 image_id with
            | Some (anx_page, _desc, _fname, key_l, _key_l_2, _occ)
              when image_id <> 0 ->
                img_ok_list := image_id :: !img_ok_list;
                let index_l =
                  List.fold_left
                    (fun acc (key : MkImgDict.key) ->
                      let sn = Sutil.replace '_' ' ' key.pk_surname in
                      let sn = Sutil.particles sn in
                      let fn = Sutil.replace '_' ' ' key.pk_first_name in
                      if sn = "Dupontxxx" && fn = "Jeanxxx" then
                        Printf.eprintf "%s, %s, photo %s\n" sn fn
                          (if anx_page <> 0 then
                             Format.sprintf "%d/%d" image_id anx_page
                           else Format.sprintf "%s" img_number0);

                      Format.sprintf "\\index{%s, %s%s, photo %s}" sn fn
                        (if key.pk_occ <> 0 then
                           Format.sprintf " (%d)" key.pk_occ
                         else "")
                        (if anx_page <> 0 then
                           Format.sprintf "%d/%d" image_id anx_page
                         else Format.sprintf "%s" img_number0)
                      :: acc)
                    [] key_l
                in
                String.concat "" index_l
            | _ -> ""
          in
          output_string och
            (Format.sprintf
               "\\parbox{%s}{\\includegraphics[width=%s]{%s%s%s}\\newline%s%s%s}%s\n"
               width width images_dir Filename.dir_sep name img_number img_label
               index_list
               (if Sutil.contains name "-wide" then "\n\\par\n" else "")))
    (List.rev images_list);
  output_string och (Format.sprintf "\\par\n")

let process_one_line conf base _dict1 _dict2 och line =
  if line <> "" then
    let line = Sutil.replace_str "%%%LIVRES%%%" !livres line in
    let line = Sutil.replace_str "%%%BASE%%%" conf.basename line in
    let line = Sutil.replace_str "%%%PASSWD%%%" conf.passwd line in
    let conf =
      match line.[0] with
      | '<' -> (
          match line.[1] with
          | 'a' | 'b' ->
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
              let key_str = Format.sprintf "%s.%s+%s" p oc n in
              let sec =
                (* -> chapter, 1-> section, ... *)
                match !current_level with
                | 0 -> ""
                | 1 ->
                    incr section;
                    mark_section och;
                    ""
                | 2 ->
                    incr subsection;
                    mark_section och;
                    "sub"
                | 3 ->
                    incr subsubsection;
                    mark_section och;
                    "subsub"
                | _ -> "subsubsub"
              in
              (* TODO review numbering and reset to 0 *)
              image_nbr :=
                if conf.imagelabels > !current_level + 1 then 0 else !image_nbr;
              image_nbr := if conf.imagelabels = 4 then 0 else !image_nbr;
              let index =
                if Sutil.contains line "\\index" then ""
                  (* index manually done *)
                else Format.sprintf "\\index{%s}" content (* automatic index *)
              in
              if line.[1] = 'a' && not !gwtest then
                output_string och
                  (Format.sprintf "\\%ssection{%s%s}\n" sec content index);

              one_http_call conf base och line;

              if conf.collectimages && !images_in_page <> [] then
                print_images conf och !images_in_page key_str;
              images_in_page := [];
              if conf.hrule && not !gwtest then
                output_string och
                  (Format.sprintf "\\par\\vspace{0.5cm}\\hrule\n");
              conf
          | 'x' -> one_command conf och line
          | 'y' ->
              if not !gwtest then output_string och "";
              conf
          | _ ->
              if not !gwtest then output_string och (line ^ "\n");
              conf)
      | _ ->
          if not !gwtest then output_string och (line ^ "\n");
          conf
    in
    conf
  else conf

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
      ("-base", Arg.String (fun x -> basename := x), " Choose base.");
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
      ( "-index",
        Arg.Int (fun x -> index := x),
        " Number of times makeindex is done." );
      ("-second", Arg.Set second, " Run Pdflatex a second time.");
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-dry_run", Arg.Set dry_run, " Don't process output.");
      ("-gwtest", Arg.Set gwtest, " GeneWeb test mode.");
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

  let conf =
    make_conf !bases !basename !passwd !family !debug !verbose !treemode
  in

  let img_file =
    String.concat Filename.dir_sep
      [ !livres; !family ^ "-inputs"; "who_is_where.txt" ]
  in

  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_txt = Filename.concat !livres (!family ^ ".txt") in
  let fname_out =
    if !test_nb <> 0 then Printf.sprintf "gwtolatex-test%d" !test_nb
    else !out_file
  in
  let mode, fname_in =
    if Sys.file_exists fname_txt then ("txt", fname_txt)
    else if Sys.file_exists fname_htm then ("html", fname_htm)
    else ("", fname_txt)
  in

  (* build images dictionnaries *)
  if !verbose then Printf.eprintf "Build images dicts\n";
  flush stderr;
  if not !gwtest then (
    let dict1_t, dict2_t, img_name_l =
      MkImgDict.create_images_dicts img_file fname_txt
    in
    dict1 := dict1_t;
    dict2 := dict2_t;
    img_name_list := img_name_l);
  if !basename = "" then (
    Arg.usage speclist usage;
    exit 2);
  let bname = Filename.concat "." !basename in
  (* TODO find a way to open base remotely *)
  try
    Driver.with_database bname @@ fun base ->
    let och = if !gwtest then Stdlib.stderr else open_out fname_out in
    let ic = open_in_bin fname_in in
    if !debug = -1 then Sys.enable_runtime_warnings false;

    if !gwtest then
      Printf.eprintf
        "This is \027[32mmkTeX\027[0m version %s running GW-test on base %s\n"
        Sutil.version conf.basename
    else
      Printf.eprintf
        "This is \027[32mmkTeX\027[0m version %s for %s on base %s to %s (%d)\n"
        Sutil.version conf.family conf.basename fname_out conf.debug;
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
        tmp := conf;
        try
          while true do
            let line = input_line ic |> Sutil.strip_nl in
            flush stderr;
            tmp := process_one_line !tmp base dict1 dict2 och line
          done
        with End_of_file ->
          close_in ic;
          close_out och));
    Printf.eprintf "\n";
    flush stderr;
    let out_channel = open_out_bin "dict1.dat" in
    Marshal.to_channel out_channel !dict1 [];
    close_out out_channel;
    img_ok_list := List.sort_uniq compare !img_ok_list;
    let out_channel = open_out_bin "list1.dat" in
    Marshal.to_channel out_channel !img_ok_list [];
    close_out out_channel
  with _ ->
    Printf.eprintf "Cannot open base %s\n" bname;
    exit 1

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
