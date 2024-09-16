(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex
open Config

type _name = string * string

(* Assumes we are running in bases folder GeneWeb security constraint *)
let gw2l_dist = ref "./gw2l_dist"
let livres = ref "../livres"
let test = ref false
let follow = ref false
let test_nb = ref 0

(* launch setup *)
let bases = ref ""
let basename = ref "x"
let passwd = ref ""
let family = ref ""
let debug = ref 0
let verbose = ref false
let treemode = ref 1

(* execution context *)
let gw_dir = ref (try Sys.getenv "GW_BIN" with Not_found -> "./")
let _passwd = ref ""
let out_file = ref ""
let dev = ref false
let dry_run = ref false
let gwtest = ref false
let second = ref false
let index = ref 0
let dict1 = ref (Hashtbl.create 100)
let dict2 = ref (Hashtbl.create 100)
let img_name_list = ref []
let img_ok_list = ref []
let missing_tags = ref []

(* TODO suppress (pages liées) and (modifier) in m=NOTES *)
(* TODO suppress "base chausey ..." *)

let letters =
  [
    "A";
    "B";
    "C";
    "D";
    "E";
    "F";
    "G";
    "H";
    "I";
    "J";
    "K";
    "L";
    "M";
    "N";
    "O";
    "P";
    "Q";
    "R";
    "S";
    "T";
    "U";
    "V";
    "W";
    "X";
    "Y";
    "Z";
  ]

let output_string_nl oc str =
  output_string oc str;
  output_string oc "\n"

let label letter =
  (Format.sprintf {|<a name="%s></a>">|} letter)

let letter_list =
  let one_letter l = Format.sprintf {|<a href="#%s>%s</a>">|} l l in
  let row = List.map (fun l -> one_letter l) letters in
  (String.concat ", " row)

let scan_person str =
  let parts = String.split_on_char '/' str in
  match List.length parts with
  | 2 -> Some (Format.sprintf "%s.0+%s" (List.nth parts 0) (List.nth parts 1))
  | 3 ->
      Some
        (Format.sprintf "%s.%s+%s" (List.nth parts 0) (List.nth parts 2)
           (List.nth parts 1))
  | 4 ->
      Some
        (Format.sprintf "%s.%s+%s" (List.nth parts 0) (List.nth parts 2)
           (List.nth parts 1))
  | _ ->
      Printf.eprintf "Funny person ref: %s\n" str;
      None

let lim = ref 0

let scan_notes notes key =
  let lines = String.split_on_char '\n' notes in
  if !lim < 4 && notes <> "" then (
    Printf.eprintf "Notes: (%s) %s\n" key notes;
    lim := succ !lim);
  let trace = key = "jean-christian.0_fresil" in
  List.fold_left
    (fun acc line ->
      if trace then Printf.eprintf "Line: %s\n" line;
      let rec loop i0 acc =
        let i = try String.index_from line i0 '[' with Not_found -> -1 in
        if i <> -1 && line.[i + 1] = '[' && line.[i + 2] <> '[' then (
          let j = try String.index_from line i ']' with Not_found -> -1 in
          if trace then Printf.eprintf "i, j: %d, %d\n" i j;
          if j = -1 || line.[j + 1] <> ']' then (
            Printf.eprintf "Bad line format: %s\n" line;
            acc)
          else
            let person = String.sub line (i + 2) (j - i - 2) in
            if trace then Printf.eprintf "Person: %s\n" person;
            match scan_person person with
            | None -> loop (j + 1) acc
            | Some key ->
                let key = Sutil.lower key |> Sutil.replace ' ' '_' in
                if trace then Printf.eprintf "Key: %s\n" key;
                loop (j + 1) (key :: acc))
        else acc
      in
      loop 0 acc)
    [] lines

let invert_table (person_ref : ('key1, 'key2 list) Hashtbl.t) :
    ('key2, 'key1 list) Hashtbl.t =
  let inv_person_ref = Hashtbl.create (Hashtbl.length person_ref) in
  Hashtbl.iter
    (fun key1 key2_list ->
      List.iter
        (fun key2 ->
          let existing =
            Hashtbl.find_opt inv_person_ref key2 |> Option.value ~default:[]
          in
          Hashtbl.replace inv_person_ref key2 (key1 :: existing))
        key2_list)
    person_ref;
  inv_person_ref

let header =
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime now in
  Format.sprintf
{|
<div style="float: right;">
  <a role="button" class="btn btn-link ml-0 p-0" href="%%s"><i class="fa fa-home fa-fw" title="Accueil" aria-hidden="true"></i><i class="sr-only">Accueil</i></a>
</div>

<h1>Index de la Généalogie chausiaise</h1>
(Mise à jour du : %04d-%02d-%02d %02d:%02d:%02d)
<br>
|} (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let print_index och new_list =
  List.fold_left (fun (iper, sn, fn, oc, aliases, kind, key_l) ->
  
  output_string och header;
  output_string och letter_list;
  
  ) "" new_list
  
  
  
let main () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ("-bases", Arg.String (fun x -> bases := x), " Where are bases.");
      ("-base", Arg.String (fun x -> basename := x), " Choose base.");
      ("-gw", Arg.String (fun x -> gw_dir := x), " Set gw folder.");
      ("-passwd", Arg.String (fun x -> passwd := x), " Set wizard password.");
      ("-family", Arg.String (fun x -> family := x), " Choose family.");
      ("-famille", Arg.String (fun x -> family := x), " Choose family.");
      ( "-livres",
        Arg.String (fun x -> livres := x),
        " Where are the families files." );
      ( "-o",
        Arg.String (fun x -> out_file := x),
        " Name of the result (default family.gw2l)." );
      ( "-index",
        Arg.Int (fun x -> index := x),
        " Number of times makeindex is done." );
      ("-second", Arg.Set second, " Run Pdflatex a second time.");
      ("-dev", Arg.Set dev, " Run in the GitHub repo.");
      ("-dry_run", Arg.Set dry_run, " Don't process output.");
      ("-gwtest", Arg.Set gwtest, " GeneWeb test mode.");
      ("-debug", Arg.Int (fun x -> debug := x), " Debug traces level.");
      ("-v", Arg.Set verbose, " Pdflatex mode (verbose or quiet).");
      ("-follow", Arg.Set follow, " Run Pdflatex.");
      ("-pdf", Arg.Set follow, " Run Pdflatex.");
      ( "-test",
        Arg.Int
          (fun x ->
            test_nb := x;
            test := true),
        " Choose test file." );
    ]
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  Arg.parse speclist anonfun usage;

  let fname_out =
    if !out_file = "" then Filename.concat "." (!family ^ "full-index.txt")
    else Filename.concat "." !out_file
  in
  Printf.eprintf
    "This is \027[32mmkIndex\027[0m version %s for %s on base %s to %s (%d)\n"
    Sutil.version !family !basename fname_out !debug;
  flush stderr;

  if !verbose then Printf.eprintf "Open base %s\n" !basename;
  (* TODO find a way to open base remotely *)
  let base = Hutil.open_base (Filename.concat "." !basename) in

  if !verbose then Printf.eprintf "Build full list\n";
  let tmp = ref [] in
  Gwdb.Collection.iter
    (fun per ->
      let iper = Gwdb.get_iper per in
      let p = Gwdb.poi base iper in
      let fn = Gwdb.sou base (Gwdb.get_first_name p) in
      let sn = Gwdb.sou base (Gwdb.get_surname p) in
      let oc = Gwdb.get_occ p in
      let aliases = Gwdb.get_aliases p in
      let notes = Gwdb.get_notes p in
      tmp := (iper, sn, fn, oc, aliases, notes) :: !tmp)
    (Gwdb.persons base);
  let full_list = !tmp in

  if !verbose then Printf.eprintf "Scan for boats\n";
  (* scan for boats *)
  let full_list =
    List.map
      (fun (iper, sn, fn, oc, aliases, notes) ->
        if sn = "X" || sn = "x" then (iper, fn, "", oc, aliases, "boat", notes)
        else (iper, sn, fn, oc, aliases, "std", notes))
      full_list
  in

  if !verbose then Printf.eprintf "Build hashtbl\n";
  let person_ref = Hashtbl.create 100 in
  List.iter
    (fun (iper, sn, fn, oc, aliases, kind, notes) ->
      let key = Format.sprintf "%s.%d+%s" fn oc sn in
      let key = Sutil.lower key |> Sutil.replace ' ' '_' in
      Hashtbl.add person_ref key (scan_notes (Gwdb.sou base notes) key))
    full_list;
  if !verbose then
    Printf.eprintf "Full_list (%d), Hashtbl (%d)\n" (List.length full_list)
      (Hashtbl.length person_ref);

  let rec loop n =
    if n > 0 then (
      let iper, sn, fn, oc, aliases, kind, notes = List.nth full_list n in
      let key = Format.sprintf "%s.%d+%s" fn oc sn in
      let key = Sutil.lower key |> Sutil.replace ' ' '_' in
      Printf.eprintf "Item: %s, (%s)\n" key
        (String.concat ", " (Hashtbl.find person_ref key));
      loop (n - 1))
  in
  loop 10;

  if !verbose then
    Printf.eprintf "Build inverse hashtbl (%d)\n" (Hashtbl.length person_ref);
  let inv_person_ref = invert_table person_ref in

  if !verbose then
    Printf.eprintf "Use inverse hashtbl (%d)\n" (Hashtbl.length inv_person_ref);
  let new_list =
    List.map
      (fun (iper, sn, fn, oc, aliases, kind, notes) ->
        let key = Format.sprintf "%s.%d+%s" fn oc sn in
        let key = Sutil.lower key |> Sutil.replace ' ' '_' in
        let key_l =
          match Hashtbl.find_opt inv_person_ref key with
          | Some key_l ->
              if !debug = 1 then
                Printf.eprintf "Found %s (%d)\n" key (List.length key_l);
              key_l
          | None -> []
        in
        (iper, sn, fn, oc, aliases, kind, key_l))
      full_list
  in

  if !verbose then Printf.eprintf "Expand aliases (%d)\n" (List.length new_list);
  let new_list =
    List.fold_left
      (fun acc (iper, sn, fn, oc, aliases, kind, notes) ->
        if aliases = [] then (iper, sn, fn, oc, aliases, kind, notes) :: acc
        else
          let al =
            List.map
              (fun al ->
                ( Gwdb.dummy_iper,
                  Gwdb.sou base al,
                  "",
                  0,
                  [],
                  "alias",
                  [ Format.sprintf "voir %s, %s" sn fn ] ))
              aliases
          in
          al @ acc)
      [] new_list
  in

  if !verbose then
    Printf.eprintf "Length with aliases: %d\n" (List.length new_list);

  if !verbose then Printf.eprintf "Sort new_list\n";

  let compare (iper1, sn1, fn1, oc1, aliases1, kind1, notes1)
      (iper2, sn2, fn2, oc2, aliases2, kind2, notes2) =
    let key1 =
      Sutil.lower (Format.sprintf "%s%s" sn1 fn1) |> Sutil.replace ' ' '_'
    in
    let key2 =
      Sutil.lower (Format.sprintf "%s%s" sn2 fn2) |> Sutil.replace ' ' '_'
    in
    if key1 < key2 then -1 else if key1 = key2 then 0 else 1
  in

  let new_list = List.sort compare new_list in

  let och = open_out fname_out in
  print_index och new_list;
  close_out och;
  Printf.eprintf "Done\n"

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
