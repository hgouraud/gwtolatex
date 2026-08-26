(* Copyright (c) 2026 H.Gouraud *)
open Gwtolatex

(* Post-pass for the <x PhotoCredits> command.

   mkTex has already:
   - emitted the marker line Sutil.photocredits_marker where the credits table
     should appear (an inert LaTeX comment until now);
   - written tmp/<family>.pcredits, one record per printed credited photo:
       aux_label<TAB>photo_number<TAB>credit

   After the first pdflatex run, tmp/<family>.aux holds, for every printed
   photo, a line
       \newlabel{img_ref_<id>.<num>}{{<ref>}{<page>}...}
   from which we read the real page number.

   This pass joins the two, groups the photos by page, sorts by page, and
   replaces the marker in tmp/<family>.tex with the rendered list. mkBook runs
   it between the two pdflatex passes; a second pdflatex then typesets the
   list. *)

let family = ref ""
let verbose = ref false
let debug = ref 0

(* ---- small helpers (no Str dependency, like the other binaries) ---- *)

(* Content of the brace group starting at [pos] (where s.[pos] = '{'), and the
   index just past its matching '}'. Depth-aware, so it stays correct even if a
   future preamble adds hyperref's nested groups. *)
let read_group s pos =
  let n = String.length s in
  if pos >= n || s.[pos] <> '{' then None
  else
    let rec loop i depth =
      if i >= n then None
      else
        match s.[i] with
        | '{' -> loop (i + 1) (depth + 1)
        | '}' ->
            if depth = 1 then Some (String.sub s (pos + 1) (i - pos - 1), i + 1)
            else loop (i + 1) (depth - 1)
        | _ -> loop (i + 1) depth
    in
    loop pos 0

(* Parse one \newlabel{img_ref_...}{{ref}{page}...} line -> (label, page). *)
let parse_newlabel line =
  let tag = "\\newlabel{" in
  let idx = Sutil.contains_index line tag in
  if idx = -1 then None
  else
    let lb = idx + String.length tag in
    match String.index_from_opt line lb '}' with
    | None -> None
    | Some le -> (
        let label = String.sub line lb (le - lb) in
        if not (Sutil.start_with "img_ref_" 0 label) then None
        else
          (* outer '{' of the second argument, then {ref}{page}... *)
          match String.index_from_opt line (le + 1) '{' with
          | None -> None
          | Some o -> (
              match read_group line (o + 1) with
              | None -> None
              | Some (_ref, p2) -> (
                  match read_group line p2 with
                  | None -> None
                  | Some (page, _) -> Some (label, String.trim page))))

let read_aux aux_file =
  let tbl = Hashtbl.create 256 in
  if Sys.file_exists aux_file then (
    let ic = open_in aux_file in
    (try
       while true do
         let line = input_line ic in
         match parse_newlabel line with
         | Some (label, page) -> Hashtbl.replace tbl label page
         | None -> ()
       done
     with End_of_file -> ());
    close_in ic);
  tbl

let read_pcredits file =
  let recs = ref [] in
  if Sys.file_exists file then (
    let ic = open_in file in
    (try
       while true do
         let line = input_line ic in
         match String.split_on_char '\t' line with
         | [ label; number; credit ] -> recs := (label, number, credit) :: !recs
         | [ label; number ] -> recs := (label, number, "") :: !recs
         | _ -> ()
       done
     with End_of_file -> ());
    close_in ic);
  List.rev !recs

(* Natural order for photo numbers like "4.11.1" (so 4.8.2 < 4.11.1). *)
let compare_number a b =
  let split s =
    List.map
      (fun p -> try int_of_string p with _ -> 0)
      (String.split_on_char '.' s)
  in
  compare (split a) (split b)

let page_int p =
  match int_of_string_opt (String.trim p) with Some n -> n | None -> max_int

(* Replace every occurrence of [sub] by [by] (all occurrences, whole file). *)
let replace_all_sub sub by s =
  let ls = String.length s and lsub = String.length sub in
  if lsub = 0 then s
  else begin
    let buf = Buffer.create ls in
    let i = ref 0 in
    while !i <= ls - lsub do
      if String.sub s !i lsub = sub then (
        Buffer.add_string buf by;
        i := !i + lsub)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.add_string buf (String.sub s !i (ls - !i));
    Buffer.contents buf
  end

(* Build the LaTeX list, one \par line per page, photos in natural order. *)
let build_list aux records =
  let unresolved = ref 0 in
  (* (page_string, number, credit) for records whose label has a page *)
  let resolved =
    List.filter_map
      (fun (label, number, credit) ->
        match Hashtbl.find_opt aux label with
        | Some page -> Some (page, number, credit)
        | None ->
            incr unresolved;
            if !verbose then
              Printf.eprintf "PhotoCredits: no page for %s (photo %s)\n" label
                number;
            None)
      records
  in
  (* sort by page then by photo number *)
  let sorted =
    List.sort
      (fun (pa, na, _) (pb, nb, _) ->
        let c = compare (page_int pa) (page_int pb) in
        if c <> 0 then c else compare_number na nb)
      resolved
  in
  (* group consecutive same-page entries *)
  let buf = Buffer.create 4096 in
  let rec group = function
    | [] -> ()
    | (page, _, _) :: _ as l ->
        let same, rest = List.partition (fun (p, _, _) -> p = page) l in
        Buffer.add_string buf
          (Format.sprintf "\\par\\noindent \\textbf{Page %s}~: " page);
        let photos =
          List.map
            (fun (_, number, credit) ->
              if credit <> "" then Format.sprintf "photo %s (%s)" number credit
              else Format.sprintf "photo %s" number)
            same
        in
        Buffer.add_string buf (String.concat ", " photos);
        Buffer.add_string buf ".\n";
        group rest
  in
  group sorted;
  (!unresolved, Buffer.contents buf)

let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-v", Arg.Set verbose, " verbose or quiet.");
    ]
  in
  let speclist = Arg.align (List.sort compare speclist) in
  Arg.parse speclist (fun _ -> ()) usage;

  let tex_file = String.concat Filename.dir_sep [ "tmp"; !family ^ ".tex" ] in
  let aux_file = String.concat Filename.dir_sep [ "tmp"; !family ^ ".aux" ] in
  let pcr_file =
    String.concat Filename.dir_sep [ "tmp"; !family ^ ".pcredits" ]
  in

  Printf.eprintf "This is \027[32mmkPhotoCredits\027[0m version %s for %s\n"
    Sutil.version !family;
  flush stderr;

  if not (Sys.file_exists tex_file) then (
    Printf.eprintf "PhotoCredits: %s not found, nothing to do\n" tex_file;
    exit 0);

  let ic = open_in_bin tex_file in
  let tex = really_input_string ic (in_channel_length ic) in
  close_in ic;

  if Sutil.contains_index tex Sutil.photocredits_marker = -1 then (
    (* the document never used <x PhotoCredits> - leave it untouched *)
    if !verbose then Printf.eprintf "PhotoCredits: no marker, nothing to do\n";
    exit 0);

  let aux = read_aux aux_file in
  let records = read_pcredits pcr_file in
  let unresolved, content = build_list aux records in

  let content =
    if content = "" then "\\par\\noindent (aucun crédit photo).\n" else content
  in
  let tex = replace_all_sub Sutil.photocredits_marker content tex in
  let oc = open_out_bin tex_file in
  output_string oc tex;
  close_out oc;

  Printf.eprintf
    "PhotoCredits: %d credited photo(s) placed, %d without a resolved page\n"
    (List.length records - unresolved)
    unresolved;
  flush stderr

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
