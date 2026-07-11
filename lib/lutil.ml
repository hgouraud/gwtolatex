(* latex utilities *)
(* v1  Henri, 2023/10/16 *)

let simple_tag_1 t str =
  (* commands that take an argument: \cmd{...} scopes correctly *)
  let arg_cmds =
    [
      ("i", "textit");
      ("u", "underline");
      ("b", "textbf");
      ("em", "textit");
      ("strong", "textbf");
      ("tt", "texttt");
    ]
  in
  (* <small>/<big> must scale RELATIVE to the surrounding size: a
     <small>1558</small> marriage year inside a FontSize-tiny tree cell
     must come out smaller than tiny, not absolute \small (which is
     LARGER than tiny). \textsmaller/\textlarger come from the relsize
     package: add \usepackage{relsize} to Gw2LaTeX-env.tex. *)
  let rel_cmds = [ ("big", "textlarger"); ("small", "textsmaller") ] in
  let arg_cmds = arg_cmds @ rel_cmds in
  (* font-size SWITCHES: must be emitted as {\cmd ...} so the switch
     cannot escape its braces. *)
  let switches = [ ("tiny", "tiny") ] in
  if str = "" then ""
  else
    match List.assoc_opt t arg_cmds with
    | Some cmd -> Format.sprintf "\\%s{%s}" cmd str
    | None -> (
        match List.assoc_opt t switches with
        | Some cmd -> Format.sprintf "{\\%s %s}" cmd str
        | None ->
            Printf.eprintf "funny tag 1 %s\n" t;
            Format.sprintf "\\underline{%s}" str)

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
