(* Copyright (c) 2013 H.Gouraud *)

let base = ref ""
let family = ref ""
let out_file = ref ""
let debug = ref false
let livres = ref "/Users/Henri/Genea/Livres"
let version oc = output_string oc "Version\n"
let one_page oc line = output_string oc line

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

let one_http_call _oc line =
  let url_beg = try String.index_from line 0 '"' with Not_found -> 0 in
  let url_end =
    try String.index_from line (url_beg + 1) '"' with Not_found -> 0
  in
  let url = String.sub line (url_beg + 1) (url_end - url_beg - 1) in
  print_string url;
  print_newline ();

  let resp = Ezcurl.get ~url () in
  begin match resp with
    | Ok {Ezcurl.code; body; _} ->
      if bad_code code then
          Printf.eprintf "bad code when fetching %s: %d\n%!" url code
      else (
          print_string
            (Format.sprintf "one_http_call done %d, %d\n" code
               (String.length body));
          print_string body)
    | Error (_, msg) ->
          Printf.eprintf "error when fetching %s:\n  %s\n%!" url msg
  end


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

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-base", Arg.String (fun x -> base := x), " Choose base.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ("-o", Arg.String (fun x -> out_file := x), " Name of the result.");
      ("-debug", Arg.Unit (fun () -> debug := true), " Debug mode.");
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;
  if not !debug then Sys.enable_runtime_warnings false;
  let out_fname = if !out_file <> "" then !out_file else !family ^ ".gw2l" in
  let oc = open_out (Filename.concat !livres out_fname) in
  let ic = open_in (Filename.concat !livres (!family ^ ".txt")) in
  try
    while true do
      let line = input_line ic in
      process_one_line oc line
    done
  with End_of_file ->
    close_in ic;
    Printf.eprintf "Done\n";
    exit 0

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
