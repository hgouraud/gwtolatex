(* Copyright (c) 2013 H.Gouraud *)

let base = ref ""
let family = ref ""
let out_file = ref ""
let debug = ref false
let livres = ref "/Users/Henri/Genea/Livres"
let version oc = output_string oc "Version\n"
let one_http_call oc line = output_string oc line
let one_page oc line = output_string oc line

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
