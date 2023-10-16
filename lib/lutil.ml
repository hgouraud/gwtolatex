(* latex utilities *)
(* v1  Henri, 2023/10/16 *)

let simple_tag_1 t str =
  let tags =
    [
      ("i", "textit");
      ("u", "underline");
      ("b", "bf");
      ("em", "it");
      ("strong", "bf");
      ("big", "large");
      ("small", "small");
      ("tiny", "tiny");
      ("tt", "tt");
    ]
  in
  let cmd =
    try List.assoc t tags
    with Not_found ->
      Printf.eprintf "funny tag 1 %s\n" t;
      "underline"
  in
  if str <> "" then Format.sprintf "{\\%s %s}" cmd str else ""

let _simple_tag_2 t str =
  let tags =
    [ ("b", "bf"); ("small", "scriptsize"); ("tiny", "tiny"); ("tt", "tt") ]
  in
  let cmd =
    try List.assoc t tags
    with Not_found ->
      Printf.eprintf "funny tag 2 %s\n" t;
      "underline"
  in
  if str <> "" then Format.sprintf "{\\%s %s}" cmd str else ""

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

let build_index fn sn ocn content check =
  (* cover cases with sn = . (houses) or X (boats) *)
  match (fn, sn) with
  | fn, "." | fn, "X" ->
      Format.sprintf "{\\bf %s}" content ^ Format.sprintf "\\index{%s}" fn
  | "Famille", sn ->
      Format.sprintf "{\\bf %s}" content ^ Format.sprintf "\\index{%s}" sn
  | fn, sn ->
      Format.sprintf "{\\bf %s}" content
      ^ Format.sprintf "\\index{%s, %s%s}" sn fn ocn
      ^
      if
        check <> content (* TODO parametrize this *)
        && sn <> "." && sn <> "X" && sn <> "?" && fn <> "?"
      then Format.sprintf "\\index{%s, voir %s, %s%s}" content sn fn ocn
      else ""
