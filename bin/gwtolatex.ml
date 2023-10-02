(* Copyright (c) 2013 H.Gouraud *)

let test = ref false
let level = ref 1
let version oc = output_string oc "Version\n"
let one_page oc line = output_string oc line

let strip_nl s =
  let b = Buffer.create 10 in
  String.iter
    (fun c -> if c = '\n' then Buffer.add_char b ' ' else Buffer.add_char b c)
    s;
  Buffer.contents b

(* returns content between matching tags, and following body *)
let find_matching_tag name body =
  let rec match_tag i body =
    let j =
      try String.index_from body i '<'
      with Not_found | Invalid_argument _ -> -1
    in
    if j = -1 then (
      Printf.eprintf "Cannot find matching tag: %s, %s\n" name
        (String.sub body 0 (min 80 (String.length body)));
      ("", ""))
    else if body.[j + 1] = '/' then
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
            (String.length body - (j + 4 + String.length name))
        in
        let body =
          if body.[0] = '\n' then String.sub body 1 (String.length body - 1)
          else body
        in
        (content, body)
      else match_tag (j + 1) body
    else match_tag (j + 1) body
  in
  match_tag 0 body

let dummy_tags_1 =
  [
    "!DOCTYPE";
    "!doc";
    "!--";
    "meta";
    "link";
    "imgsrc";
    "fontcolor";
    "input";
    "option";
    "col";
    "html";
    "/html";
    "body";
    "/body";
    "tbody";
    "/tbody";
    "div";
    "/div";
    "bdo";
    "/bdo";
  ]

let dummy_tags_2 =
  [
    "head";
    "form";
    "select";
    "colgroup";
    "span";
    "samp";
    "font";
    "script";
  ]

(* tag is <name atributes> may be multi lines *)

let rec process_html oc body =
  if !level > 1 then
    Printf.eprintf "Process html %s\n" (String.sub body 0 (min 80 (String.length body)));
  let process_tag oc tag body =
    if !level > 1 then Printf.eprintf "Process tag: %s\n" tag;
    let tag = strip_nl tag in
    let name, attr =
      let j =
        try String.index_from tag 0 '>'
        with Not_found | Invalid_argument _ -> -1
      in
      if j = -1 then (
        Printf.eprintf "Bad tag: %s\n" tag;
        exit 1);
      let j =
        try String.index_from tag 0 ' '
        with Not_found | Invalid_argument _ -> -1
      in
      if j = -1 then (String.sub tag 1 (String.length tag - 2), "")
      else
        ( String.sub tag 1 (j - 1),
          String.sub tag (j + 1) (String.length tag - j - 2) )
    in
    let attr = String.split_on_char ' ' attr in
    let attr =
      List.map
        (fun a ->
          let eq = try String.index_from a 0 '=' with Not_found -> 0 in
          let k = String.sub a 0 eq in
          let v = String.sub a eq (String.length a - eq) in
          let v =
            if v <> "" && v.[0] = '=' then String.sub v 1 (String.length v - 1)
            else v
          in
          (k, v))
        attr
    in
    if !test && !level > 2 then Printf.eprintf "Tag: %s\n" tag;
    match name with
    | tag when List.mem tag dummy_tags_1 -> body
    | tag when List.mem tag dummy_tags_2 ->
        let _content, body = find_matching_tag tag body in
        body
    | "a" -> (
        let href = try List.assoc "href" attr with Not_found -> "no href" in
        let content, body = find_matching_tag "a" body in
        output_string oc ("<a>" ^ href ^ " -> ");
        if !level > 1 then 
          Printf.eprintf "\nContent: %s\n" content;
        process_html oc content;
        output_string oc "</a>";
        body)
    | "img" ->
        let src = try List.assoc "src" attr with Not_found -> "no src" in
        output_string oc ("<img " ^ src ^ ">");
        body
    | _ ->
        output_string oc (Printf.sprintf "other tags: %s\n" tag);
        body
  in
  if body = "" then ()
  else if body.[0] <> '<' then
    let j =
      try String.index_from body 0 '<'
      with Not_found | Invalid_argument _ -> -1
    in
    if j = -1 then output_string oc body
    else (
      output_string oc (String.sub body 0 j);
      process_html oc (String.sub body j (String.length body - j)))
  else (
    let tag, body =
      let j =
        try String.index_from body 0 '>'
        with Not_found | Invalid_argument _ -> -1
      in
      ( String.sub body 0 (j + 1),
        String.sub body (j + 1) (String.length body - j - 1) )
    in
    if !level > 1 then
      Printf.eprintf "Tag: %s, (%s)\n" tag body ;
    let body =
      if body.[0] = '\n' then String.sub body 1 (String.length body - 1)
      else body
    in
    let body =
      if tag <> "" || body <> "" then process_tag oc tag body else "xxx"
    in
    process_html oc body)

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
      else process_html oc body
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
      ("-test", Arg.Set test, " Pdflatex mode (batch or not).");
      ("-debug", Arg.Unit (fun () -> debug := true), " Debug mode.");
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;
  if not !debug then Sys.enable_runtime_warnings false;
  let out_fname = if !out_file <> "" then !out_file else !family ^ ".gw2l" in
  (if !test then (
   let ic = open_in_bin "test/gwtolatex-test.html" in
   let oc = stderr in
   let body = really_input_string ic (in_channel_length ic) in
   Printf.eprintf "\n***Starting test\n";
   process_html oc body;
   close_in ic;
   exit 0)
  else
    let oc = open_out (Filename.concat !livres out_fname) in
    let ic = open_in (Filename.concat !livres (!family ^ ".txt")) in
    try
      while true do
        let line = input_line ic in
        process_one_line oc line
      done
    with End_of_file ->
      close_in ic;
      close_out oc);
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
