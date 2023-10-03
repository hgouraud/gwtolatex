(* Copyright (c) 2013 H.Gouraud *)

let test = ref false
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

let _dummy_tags_1 =
  [
    "!--";
    "body";
    "/body";
    "bdo";
    "/bdo";
    "col";
    "div";
    "/div";
    "!DOCTYPE";
    "!doc";
    "html";
    "/html";
    "imgsrc";
    "fontcolor";
    "input";
    "link";
    "meta";
    "nav";
    "option";
    "samp";
    "/samp";
    "span";
    "/span";
    "tbody";
    "/tbody";
  ]

let _dummy_tags_2 =
  [ "button"; "head"; "form"; "select"; "colgroup"; "font"; "script" ]

type my_tree = Text of string | Element of string * (string * string) list * my_tree list

let rec process_tree oc tree =
  match tree with
  | Text s -> output_string oc s
  | Element (name, attributes, children) ->
    match name with
    | "i" -> (output_string oc "\\i{"; process_tree oc children; output_string oc "}")
    | _ -> (output_string oc "<name"; process_tree oc children; output_string oc ">")

let process_html oc body =
  let open Markup in
  body |> string
  |> parse_html |> signals
  |> tree
  ~text:(fun ss -> Text (String.concat "" ss))
  ~element:(fun (name, _) attributes children -> Element (name, attributes, children))
  |> process_tree oc

let bad_code c = c >= 400

let one_command oc command =
  let out c command =
    output_string oc
      (c
      ^ String.sub command (String.length c)
          (String.length command - String.length c)
      ^ "\n")
  in
  let parts = String.split_on_char ' ' command in
  match List.nth parts 0 with
  | "Chapter" as c -> out c command
  | "Section" as c -> out c command
  | _ -> output_string oc (command ^ "\n")

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
      | 'x' -> one_command oc (String.sub line 3 (String.length line - 5))
      | 'y' -> output_string oc ""
      | _ -> output_string oc line)
  | _ -> output_string oc line

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
  let fname_txt = Printf.sprintf "test/gwtolatex-test%d.txt" !test_nb in
  let fname_htm = Printf.sprintf "test/gwtolatex-test%d.html" !test_nb in
  let fname_all = Filename.concat !livres (!family ^ ".txt") in
  let fname_out = if !out_file <> "" then !out_file else !family ^ ".gw2l" in
  let mode, fname_in, oc =
    if Sys.file_exists fname_txt then ("txt", fname_txt, stderr)
    else if Sys.file_exists fname_htm then ("html", fname_htm, stderr)
    else ("", fname_all, open_out (Filename.concat !livres fname_out))
  in
  let ic = open_in_bin fname_in in
  if not !debug then Sys.enable_runtime_warnings false;
  if !test then Printf.eprintf "\n***Starting test with %s\n" fname_in;
  (match mode with
  | "html" ->
      let body = really_input_string ic (in_channel_length ic) in
      process_html oc body;
      close_in ic;
      close_out oc;
      exit 0
  | _ -> (
      try
        while true do
          let line = input_line ic in
          process_one_line oc line
        done
      with End_of_file ->
        close_in ic;
        close_out oc;
        if !test then exit 0 else ()));
  Printf.eprintf "Done\n";

  let mode = if !batch then "" else "-interaction=batchmode" in
  let cmmd =
    Printf.sprintf "pdflatex %s %s.tex" mode (Filename.concat !livres !family)
  in
  let error = Sys.command cmmd in
  if error <> 0 then (
    Printf.eprintf "Error in pdflatex processing (%d)\n" error;
    exit 0);
  let cmmd =
    Printf.sprintf "makeindex %s %s.tex" mode (Filename.concat !livres !family)
  in
  for _i = 0 to !index do
    let error = Sys.command cmmd in
    if error <> 0 then (
      Printf.eprintf "Error in makeindex processing (%d)\n" error;
      exit 0)
  done

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
