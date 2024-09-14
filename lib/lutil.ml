(* latex utilities *)
(* v1  Henri, 2023/10/16 *)

let simple_tag_1 t str =
  let tags =
    [
      ("i", "textit");
      ("u", "underline");
      ("b", "textbf");
      ("em", "it");
      ("strong", "textbf");
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
  if str <> "" then Format.sprintf "\\%s{%s}" cmd str else ""

let escape_aux str =
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

(*\includegraphics[width=1.50cm]{./images/chausey/andre.0.fauchon_villeplee.jpg}*)
(* Do not escape _ in file names !! *)
let escape str =
  let i = Sutil.contains_index str "\\includegraphics" in
  if i = -1 then escape_aux str
  else
    let rec loop str1 str2 =
      let i = Sutil.contains_index str1 "\\includegraphics" in
      if i = -1 then str2 ^ escape_aux str1
      else
        let j = try String.index_from str1 i '}' with Not_found -> -1 in
        if j = -1 then str1 ^ str2
        else
          loop
            (String.sub str1 (j + 1) (String.length str1 - j - 1))
            (escape_aux (String.sub str1 0 i)
            ^ String.sub str1 i (j - i + 1)
            ^ str2)
    in
    loop str ""
