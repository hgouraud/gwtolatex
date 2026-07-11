(* latex utilities *)
(* v1  Henri, 2023/10/16 *)

let simple_tag_1 t str =
  let tags =
    [
      ("i", "textit");
      ("u", "underline");
      ("b", "textbf");
      (* \it is a font switch, not a one-arg command: \it{x} italicizes
         everything after it. \textit scopes correctly. Same for tt/large
         etc. below if they ever misbehave. *)
      ("em", "textit");
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
(*\input{./images/chausey/some_file}*)
(* Do not escape the argument of these commands: it is a raw file name or
   path, not GeneWeb text - see also mkTex.ml's handling of \includegraphics
   arguments in image filenames. Add more command names here as needed. *)
let protected_commands = [ "\\includegraphics"; "\\input" ]

let escape str =
  let find_first str1 =
    List.fold_left
      (fun acc cmd ->
        match Sutil.contains_index str1 cmd with
        | -1 -> acc
        | i -> ( match acc with Some j when j <= i -> acc | _ -> Some i))
      None protected_commands
  in
  let rec loop str1 acc =
    match find_first str1 with
    | None -> acc ^ escape_aux str1
    | Some i -> (
        let j = try String.index_from str1 i '}' with Not_found -> -1 in
        match j with
        | -1 -> acc ^ str1
        | _ ->
            loop
              (String.sub str1 (j + 1) (String.length str1 - j - 1))
              (acc
              ^ escape_aux (String.sub str1 0 i)
              ^ String.sub str1 i (j - i + 1)))
  in
  loop str ""
