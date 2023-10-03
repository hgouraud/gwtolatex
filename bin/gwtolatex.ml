(* Copyright (c) 2013 H.Gouraud *)

let test = ref false
let follow = ref false
let test_nb = ref 0
let level = ref 1
let version oc = output_string oc "Version\n"
let one_page oc line = output_string oc line

let _strip_nl s =
  let b = Buffer.create 10 in
  String.iter
    (fun c -> if c = '\n' then Buffer.add_char b ' ' else Buffer.add_char b c)
    s;
  Buffer.contents b

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

let dummy_tags_1 =
  [ "!--"; "body"; "bdo"; "samp"; "span"; "table"; "tbody"; "div"; "html" ]

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
  [ "button"; "head"; "form"; "select"; "colgroup"; "font"; "script" ]

type name = string * string

type my_tree =
  | Text of string
  | Element of string * (name * string) list * my_tree list

let rec process_tree oc tree =
  match tree with
  | Text s -> output_string oc s
  | Element (name, attributes, children) -> (
      match name with
      | ("i" | "b") as t ->
          output_string oc (Format.sprintf "{\\%s " t);
          List.iter (fun c -> process_tree oc c) children;
          output_string oc "}"
      | "p" ->
          output_string oc (Format.sprintf "\\par");
          List.iter (fun c -> process_tree oc c) children
      | "ul" ->
          output_string oc (Format.sprintf "\\begin{hgitemize}");
          List.iter (fun c -> process_tree oc c) children;
          output_string oc "\\end{hgitemize}"
      | "li" ->
          output_string oc (Format.sprintf "\\item ");
          List.iter (fun c -> process_tree oc c) children
      | "small" ->
          output_string oc (Format.sprintf "{\\small ");
          List.iter (fun c -> process_tree oc c) children;
          output_string oc "}"
      | name when List.mem name dummy_tags_1 ->
          List.iter (fun c -> process_tree oc c) children
      | name when List.mem name dummy_tags_2 -> ()
      | name when List.mem name dummy_tags_3 -> ()
      | a ->
          output_string oc
            (Printf.sprintf "<begin %s %d (%s)>" a (List.length attributes)
               (if List.length attributes > 0 then
                List.fold_left
                  (fun acc ((a, b), c) -> acc ^ a ^ ":" ^ b ^ "=" ^ c)
                  "" attributes
               else ""));
          List.iter (fun c -> process_tree oc c) children;
          output_string oc (Printf.sprintf "<end %s>" a))

let process_html oc body =
  let open Markup in
  let tree =
    body |> string |> parse_html |> signals
    |> tree
         ~text:(fun ss -> Text (String.concat "" ss))
         ~element:(fun (_, name) attributes children ->
           Element (name, attributes, children))
  in
  match tree with Some tree -> process_tree oc tree | _ -> failwith "bad tree"

let bad_code c = c >= 400

let one_command oc line =
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
    output_string oc (Format.sprintf "\\%s{%s}%s\n" c param remain)
  in

  let parts = String.split_on_char ' ' cmd in
  match List.nth parts 0 with
  | "Chapter" -> out "chapter" cmd
  | "Section" -> out "section" cmd
  | _ -> output_string oc (Format.sprintf "%%%s%s\n" cmd remain)

let one_http_call oc line =
  let url_beg = try String.index_from line 0 '"' with Not_found -> 0 in
  let url_end =
    try String.index_from line (url_beg + 1) '"' with Not_found -> 0
  in
  let url = String.sub line (url_beg + 1) (url_end - url_beg - 1) in
  output_string oc url;
  output_string oc "\n";
  let resp = Ezcurl.get ~url () in
  match resp with
  | Ok { Ezcurl.code; body; _ } ->
      if bad_code code then
        Printf.eprintf "bad code when fetching %s: %d\n%!" url code
      else (
        if !level > 1 then Printf.eprintf "Body size: %d\n" (String.length body);
        process_html oc body)
  | Error (_, msg) -> Printf.eprintf "error when fetching %s:\n  %s\n%!" url msg

let process_one_line oc line =
  match line.[0] with
  | '<' -> (
      match line.[1] with
      | 'v' -> version oc
      | 'a' -> one_http_call oc line
      | 'b' -> one_page oc line
      | 'x' -> one_command oc line
      | 'y' -> output_string oc ""
      | 'z' -> one_command oc line
      | _ -> output_string oc (line ^ "\n"))
  | _ -> output_string oc (line ^ "\n")

let base = ref ""
let family = ref ""
let out_file = ref ""
let debug = ref false
let index = ref 0
let batch = ref false
let livres = ref "/Users/Henri/Genea/Livres"
let bases = ref "/Users/Henri/Genea/GeneWeb-Bases"

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
    Printf.sprintf "test/gwtolatex-test%d.txt" !test_nb,
    if !family <> "" then !family else Printf.sprintf "gwtolatex-test%d" !test_nb
  in
  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_all = Filename.concat !livres (!family ^ ".txt") in
  let fname_out = if !out_file <> "" then !out_file else family_out ^ ".tex" in
  let mode, fname_in, oc =
    if Sys.file_exists fname_txt then
      ( "txt",
        fname_txt,
        if !follow then open_out fname_out else stderr
      )
    else if Sys.file_exists fname_htm then ("html", fname_htm, stderr)
    else ("", fname_all, open_out (Filename.concat !livres fname_out))
  in
  let ic = open_in_bin fname_in in
  if not !debug then Sys.enable_runtime_warnings false;
  if !test then (Printf.eprintf "\n***Starting test with %s\n" fname_in; flush stderr);
  (match mode with
  | "html" ->
      let body = really_input_string ic (in_channel_length ic) in
      process_html oc body;
      close_in ic;
      close_out oc;
      exit 0
  | _ ->
      try
        while true do
          let line = input_line ic in
          process_one_line oc line
        done
      with End_of_file -> (
        close_in ic;
        close_out oc));
  Printf.eprintf "Done txt parsing\n"; flush stderr;

  let mode = if !batch then "" else "-interaction=batchmode" in
  let cmmd1 =
    Printf.sprintf "pdflatex %s %s.tex" mode family_out
  in
  let error = Sys.command cmmd1 in
  if error <> 0 then (
    Printf.eprintf "Error in pdflatex processing (%d)\n" error;
    exit 0);
  if !test then exit 0;
  (* makeindex does not like absolute paths! *)
  let cmmd2 =
    Printf.sprintf "makeindex %s.idx" family_out
  in
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
