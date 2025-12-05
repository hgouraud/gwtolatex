(* Copyright (c) 2013 H.Gouraud *)
open Gwtolatex
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type _name = string * string

(* Assumes we are running in bases folder GeneWeb security constraint *)
let _gw2l_dist = ref "./gw2l_dist"
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
let _treemode = ref 1

(* execution context *)
let gw_dir = ref (try Sys.getenv "GW_BIN" with Not_found -> "./")
let _passwd = ref ""
let out_file = ref ""
let dev = ref false
let dry_run = ref false
let gwtest = ref false
let second = ref false
let index = ref 0
let _dict1 = ref (Hashtbl.create 100)
let _dict2 = ref (Hashtbl.create 100)
let _img_name_list = ref []
let _img_ok_list = ref []
let _missing_tags = ref []

(* TODO suppress (pages liées) and (modifier) in m=NOTES *)
(* TODO suppress "base chausey ..." *)

let nbc c =
  if Char.code c < 0x80 then 1
  else if Char.code c < 0xC0 then assert false
  else if Char.code c < 0xE0 then 2
  else if Char.code c < 0xF0 then 3
  else if Char.code c < 0xF8 then 4
  else if Char.code c < 0xFC then 5
  else if Char.code c < 0xFE then 6
  else assert false

let build_letters list =
  let letters =
    let rec loop acc list =
      match list with
      | [] -> acc
      | (_iper, sn, _fn, _oc, _aliases, _kind, _key_l) :: list ->
          let n = nbc sn.[0] in
          let c = String.sub (Sutil.particles sn) 0 n in
          if List.mem c acc || n > 1 || c > "a" then loop acc list
          else loop (c :: acc) list
    in
    loop [] list
  in
  List.sort compare letters

let _output_string_nl oc str =
  output_string oc str;
  output_string oc "\n"

let label letter = Format.sprintf "<a name=\"%s\"></a>\n" letter

let letters_list letters =
  let one_letter l = Format.sprintf {|<a href="#%s">%s</a>|} l l in
  let row = List.map (fun l -> one_letter l) letters in
  String.concat ", " row ^ "<br>\n"

(* from Geneweb.Gutil *)
let designation base p =
  let first_name = Driver.p_first_name base p in
  let nom = Driver.p_surname base p in
  first_name ^ "." ^ string_of_int (Driver.get_occ p) ^ " " ^ nom

(* Wiki format fn/sn/oc/tet *)
let scan_person base str =
  let parts = String.split_on_char '/' str in
  let fn, oc, sn =
    match List.length parts with
    | 2 -> (List.nth parts 0, 0, List.nth parts 1)
    | 3 ->
        (* si 2ns champ n'est pas numérique, c'est du texte *)
        (List.nth parts 0, 0, List.nth parts 1)
    | 4 -> (List.nth parts 0, int_of_string (List.nth parts 2), List.nth parts 1)
    | _ ->
        Printf.eprintf "Funny person ref: %s\n" str;
        ("", 0, "")
  in
  Driver.person_of_key base fn sn oc

(* Key format fn.oc+sn *)
let scan_person2 base str =
  let parts = String.split_on_char '.' str in
  let fn = List.nth parts 0 in
  let i =
    try String.index_from (List.nth parts 1) 0 '_' with Not_found -> -1
  in
  if i = -1 then (
    Printf.eprintf "Str: (%s)\n" str;
    assert false);
  if !debug = 5 then Printf.eprintf "Str: (%s)\n" str;
  let oc = int_of_string (String.sub (List.nth parts 1) 0 i) in
  let sn =
    String.sub (List.nth parts 1) (i + 1)
      (String.length (List.nth parts 1) - i - 1)
  in
  Driver.person_of_key base fn sn oc

let scan_notes base notes key =
  let lines = String.split_on_char '\n' notes in
  let trace = key = "xxxxvictor.0_lepaisant" in
  if trace then Printf.eprintf "\n\nNotes for %s\n" key;
  let res =
    List.fold_left
      (fun acc line ->
        if trace && false then Printf.eprintf "Line: %s\n" line;
        let rec loop i0 acc =
          let i = try String.index_from line i0 '[' with Not_found -> -1 in
          if i <> -1 && line.[i + 1] = '[' && line.[i + 2] <> '[' then (
            let j = try String.index_from line i ']' with Not_found -> -1 in
            if trace && false then Printf.eprintf "i, j: %d, %d\n" i j;
            if j = -1 || line.[j + 1] <> ']' then (
              Printf.eprintf "Bad line format: %s\n" line;
              acc)
            else
              let person = String.sub line (i + 2) (j - i - 2) in
              if trace then Printf.eprintf "Person: %s\n" person;
              match scan_person base person with
              | Some ip when not (List.mem ip acc) -> loop (j + 1) (ip :: acc)
              | _ -> loop (j + 1) acc)
          else acc
        in
        loop 0 acc)
      [] lines
  in
  if trace then
    Printf.eprintf "List: %s\n"
      (List.map (fun ip -> designation base (Driver.poi base ip)) res
      |> String.concat ", ");
  res

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
(Mise à jour du : %04d-%02d-%02d %02d:%02d)
<br>
|}
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

let print_one_item och base inv_person_ref
    (iper, sn, fn, oc, _aliases, kind, key_l) =
  let p = Driver.poi base iper in
  let fn_a = Driver.sou base (Driver.get_first_name p) in
  let sn_a = Driver.sou base (Driver.get_surname p) in
  let oc_a = Driver.get_occ p in
  let fn1, oc1, sn1, sn2 =
    match kind with
    | "boat" -> (fn, oc, "X", "")
    | "list" -> (fn, oc, ".", "")
    | "alias" -> (fn, oc, sn, Sutil.particles sn)
    | _ -> (fn, oc, sn, Sutil.particles sn ^ ", ")
  in
  let key1 =
    Format.sprintf "%s%s%s" sn2 fn1
      (if oc1 <> 0 then Format.sprintf ".%d" oc1 else "")
  in
  let key2 =
    Format.sprintf "%s%s%s" sn_a fn_a
      (if oc_a <> 0 then Format.sprintf ".%d" oc_a else "")
  in
  let p_a, n_a =
    ( Format.sprintf "%s%s" fn_a
        (if oc_a = 0 then "" else "." ^ string_of_int oc_a),
      sn_a )
  in
  let p_1, n_1 =
    ( Format.sprintf "%s%s" fn1 (if oc1 = 0 then "" else "." ^ string_of_int oc1),
      sn1 )
  in

  if key1 <> key2 then (
    output_string och
      (Format.sprintf "<b><a href=\"%%sm=S;p=%s;n=%s\">%s%s%s</a></b>\n"
         (if kind = "alias" then p_a else p_1)
         (if kind = "alias" then n_a else n_1)
         sn2 fn1
         (if oc1 <> 0 then Format.sprintf ".%d" oc1 else ""));

    (match kind with
    | "std" ->
        if List.length key_l > 0 then
          let person_ref_l = Hashtbl.find inv_person_ref iper in
          let person_ref_l =
            List.map
              (fun key ->
                match scan_person2 base key with
                | Some ip ->
                    let p = Driver.poi base ip in
                    let fn = Driver.sou base (Driver.get_first_name p) in
                    let sn = Driver.sou base (Driver.get_surname p) in
                    let oc = Driver.get_occ p in
                    let p_0, n_0 =
                      ( Format.sprintf "%s%s" fn
                          (if oc = 0 then "" else "." ^ string_of_int oc),
                        sn )
                    in
                    let sn = if sn = "X" || sn = "." then "" else sn in
                    Format.sprintf "<a href=\"%%sm=S;p=%s;n=%s\">%s%s %s</a>"
                      p_0 n_0 fn
                      (if oc = 0 then "" else "." ^ string_of_int oc)
                      sn
                | None -> Format.sprintf "%s.%d %s" fn oc sn)
              person_ref_l
          in
          output_string och
            (Format.sprintf
               " <small>cité(e) %d fois dans&nbsp;:</small><br>\n\
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%s<br>\n"
               (List.length person_ref_l)
               (String.concat ", " person_ref_l))
        else output_string och "<br>"
    | "boat" -> output_string och " <small><em>(bateau)</em></small><br>\n"
    | "list" -> output_string och " <small><em>(liste)</em></small><br>\n"
    | "alias" ->
        output_string och
          (Format.sprintf
             " <small><em>(surnom ou alias de : <a \
              href=\"%%sm=S;p=%s;n=%s\">%s%s%s</a>)</em></small><br>\n"
             p_a n_a
             (if sn_a = "X" || sn_a = "x" then "" else sn_a ^ ", ")
             fn_a
             (if oc_a <> 0 then Format.sprintf ".%d" oc_a else ""))
    | s -> Printf.eprintf "Funny kind: %s\n" s);
    output_string och "\n")

let print_list_head n liste =
  let rec loop n = function
    | [] -> ()
    | _ when n < 0 -> ()
    | (_iper, sn, fn, oc, _aliases, _kind, _notes) :: liste ->
        Printf.eprintf "Item: %s %s.%d\n" sn fn oc;
        loop (n - 1) liste
  in
  loop n liste

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
    if !out_file = "" then Filename.concat "." (!family ^ "-full-index.txt")
    else Filename.concat "." !out_file
  in
  Printf.eprintf
    "This is \027[32mmkIndex\027[0m version %s for %s on base %s to %s (%d)\n"
    Sutil.version !family !basename fname_out !debug;
  flush stderr;

  if !verbose then Printf.eprintf "Open base %s\n" !basename;
  if !basename = "" then (
    Arg.usage speclist usage;
    exit 2);
  let bname = Filename.concat "." !basename in
  (* TODO find a way to open base remotely *)
  try
    Driver.with_database bname @@ fun base ->
    if !verbose then Printf.eprintf "Build full list\n";
    let tmp = ref [] in
    Collection.iter
      (fun per ->
        let iper = Driver.get_iper per in
        let p = Driver.poi base iper in
        let fn = Driver.sou base (Driver.get_first_name p) in
        let sn = Driver.sou base (Driver.get_surname p) in
        let oc = Driver.get_occ p in
        let aliases = Driver.get_aliases p in
        let notes = Driver.get_notes p in
        if !debug = 4 && sn = "Marie" && fn = "Albert" then
          Printf.eprintf "Found Albert\n";
        tmp := (iper, sn, fn, oc, aliases, notes) :: !tmp)
      (Driver.persons base);
    let full_list = !tmp in

    if !verbose then Printf.eprintf "Scan for boats\n";
    (* scan for boats *)
    let full_list =
      List.map
        (fun (iper, sn, fn, oc, aliases, notes) ->
          if sn = "X" || sn = "x" then (iper, fn, fn, oc, aliases, "boat", notes)
          else (iper, sn, fn, oc, aliases, "std", notes))
        full_list
    in

    if !verbose then Printf.eprintf "Scan for .\n";
    (* scan for . *)
    let full_list =
      List.map
        (fun (iper, sn, fn, oc, aliases, kind, notes) ->
          if sn = "." then (iper, fn, fn, oc, aliases, "list", notes)
          else (iper, sn, fn, oc, aliases, kind, notes))
        full_list
    in

    if !verbose then Printf.eprintf "Scan for ? ?\n";
    (* scan for ? ? *)
    let full_list =
      List.fold_left
        (fun acc (iper, sn, fn, oc, aliases, kind, notes) ->
          if sn = "?" then acc
          else (iper, sn, fn, oc, aliases, kind, notes) :: acc)
        [] full_list
    in

    if !verbose then Printf.eprintf "Build person_ref hashtbl\n";
    let person_ref = Hashtbl.create 100 in
    List.iter
      (fun (_iper, sn, fn, oc, _aliases, kind, notes) ->
        let sn = match kind with "boat" -> "X" | "list" -> "." | _ -> sn in
        let key = Format.sprintf "%s.%d+%s" fn oc sn in
        let key = Sutil.lower key |> Sutil.replace ' ' '_' in
        Hashtbl.add person_ref key (scan_notes base (Driver.sou base notes) key))
      full_list;
    if !verbose then
      Printf.eprintf "Full_list (%d), Hashtbl (%d)\n" (List.length full_list)
        (Hashtbl.length person_ref);

    let rec loop n =
      if n > 0 then (
        let _iper, sn, fn, oc, _aliases, _kind, _notes = List.nth full_list n in
        let key = Format.sprintf "%s.%d+%s" fn oc sn in
        let key = Sutil.lower key |> Sutil.replace ' ' '_' in
        Printf.eprintf "Hashtbl item: %s, (%s)\n" key
          (List.map
             (fun ip -> designation base (Driver.poi base ip))
             (Hashtbl.find person_ref key)
          |> String.concat ", ");
        loop (n - 1))
    in
    if !debug = 3 then loop 10;

    (if !debug = 4 then
       let key1 = "albert.0_marie" in
       let key2 = "albert.1_marie" in
       if Hashtbl.mem person_ref key1 || Hashtbl.mem person_ref key2 then
         Printf.eprintf "Albert in person_ref\n"
       else Printf.eprintf "Albert not person_ref\n");

    if !debug = 4 then
      List.iter
        (fun (_iper, sn, fn, oc, _aliases, _kind, _notes) ->
          if sn = "Marie" && fn = "Albert" then
            Printf.eprintf "Albert in full_list (%d)\n" oc;
          if sn = "." then
            Printf.eprintf "Found in full_list %s.%d %s\n" fn oc sn)
        full_list;

    if !verbose then
      Printf.eprintf "Build inverse hashtbl (%d)\n" (Hashtbl.length person_ref);
    let inv_person_ref = invert_table person_ref in

    if !verbose then
      Printf.eprintf "Build ref lists (%d)\n" (Hashtbl.length inv_person_ref);
    let new_list =
      List.map
        (fun (iper, sn, fn, oc, aliases, kind, _notes) ->
          let sn2 = if kind = "boat" then "X" else sn in
          let sn2 = if kind = "list" then "." else sn2 in
          match Driver.person_of_key base fn sn2 oc with
          | Some ip ->
              let key_l =
                match Hashtbl.find_opt inv_person_ref ip with
                | Some key_l ->
                    if !debug = 2 then
                      Printf.eprintf "Found %s (%d)\n"
                        (Driver.Iper.to_string ip) (List.length key_l);
                    key_l
                | None -> []
              in
              (iper, sn, fn, oc, aliases, kind, key_l)
          | None ->
              Printf.eprintf "Ip None %s, %s\n" fn sn;
              (iper, sn, fn, oc, aliases, kind, []))
        full_list
    in

    if !debug = 4 then
      List.iter
        (fun (_iper, sn, fn, oc, _aliases, _kind, _notes) ->
          if sn = "Marie" && fn = "Albert" then
            Printf.eprintf "Albert in new_list (%d)\n" oc)
        new_list;

    if !verbose then
      Printf.eprintf "Expand aliases (%d)\n" (List.length new_list);
    let new_list =
      List.fold_left
        (fun acc (iper, sn, fn, oc, aliases, kind, notes) ->
          if aliases = [] then (iper, sn, fn, oc, aliases, kind, notes) :: acc
          else
            let al =
              List.map
                (fun al ->
                  ( iper,
                    Driver.sou base al,
                    "",
                    0,
                    [],
                    "alias",
                    [
                      Format.sprintf "voir %s, %s.%s" sn fn
                        (if oc = 0 then "" else "." ^ string_of_int oc);
                    ] ))
                aliases
            in
            (iper, sn, fn, oc, aliases, kind, notes) :: (al @ acc))
        [] new_list
    in

    if !verbose then
      Printf.eprintf "Length with aliases: %d\n" (List.length new_list);

    if !debug = 3 then print_list_head 20 new_list;

    if !debug = 4 then
      List.iter
        (fun (_iper, sn, fn, oc, _aliases, _kind, _notes) ->
          if sn = "Marie" && fn = "Albert" then
            Printf.eprintf "Albert in new_list_2 (%d)\n" oc;
          if sn = "." then
            Printf.eprintf "Found in new_list_2 %s.%d %s\n" fn oc sn)
        new_list;

    if !verbose then Printf.eprintf "Sort new_list\n";

    let compare (_iper1, sn1, fn1, oc1, _aliases1, _kind1, _notes1)
        (_iper2, sn2, fn2, oc2, _aliases2, _kind2, _notes2) =
      let sn1 = Sutil.particles sn1 in
      let sn2 = Sutil.particles sn2 in
      let key1 =
        Sutil.lower (Format.sprintf "%s%s%d" sn1 fn1 oc1)
        |> Sutil.replace ' ' '_'
      in
      let key2 =
        Sutil.lower (Format.sprintf "%s%s%d" sn2 fn2 oc2)
        |> Sutil.replace ' ' '_'
      in
      if key1 = "MarieAlbert0" || key1 = "MarieAlbert1" then
        Printf.eprintf "Sorted Albert\n";
      if key1 < key2 then -1 else if key1 = key2 then 0 else 1
    in

    let old = List.length new_list in
    let new_list = List.sort_uniq compare new_list in
    if !verbose then
      Printf.eprintf "New list (sort uniq) %d -> %d\n" old
        (List.length new_list);

    if !debug = 4 then
      List.iter
        (fun (_iper, sn, fn, oc, _aliases, _kind, _notes) ->
          if sn = "Marie" && fn = "Albert" then
            Printf.eprintf "Albert in new_list_3 (%d)\n" oc)
        new_list;

    let letters = build_letters new_list in
    if !verbose then Printf.eprintf "Letters: %d\n" (List.length letters);

    if !debug = 3 then print_list_head 20 new_list;

    let print_index och base new_list _person_ref letters =
      output_string och header;
      output_string och "<p>";
      output_string och (label (List.hd letters));
      output_string och (letters_list letters);
      output_string och "<dl>";
      let rec loop1 ltrs new_list =
        match ltrs with
        | [] -> ()
        | letter :: ltrs ->
            let ll = (Sutil.lower letter).[0] in
            let rec loop2 new_list =
              match new_list with
              | [] -> output_string och "</dl>"
              | ((_iper, sn, _fn, _oc, _aliases, _kind, _key_l) as item)
                :: new_list
                when String.length sn > 0
                     && sn <> "?"
                     && (Sutil.lower (Sutil.particles sn)).[0] = ll ->
                  print_one_item och base inv_person_ref item;
                  loop2 new_list
              | ((_iper, _sn, _fn, _oc, _aliases, _kind, _key_l) as item)
                :: new_list
                when List.length ltrs > 0 ->
                  output_string och "</dl><p>";
                  output_string och (label (List.hd ltrs));
                  output_string och (letters_list letters);
                  output_string och "<dl>";
                  print_one_item och base inv_person_ref item;
                  loop1 ltrs new_list
              | _ -> ()
            in
            loop2 new_list
      in
      loop1 letters new_list
    in

    (if !debug = 4 then
       let key1 = "albert.0_marie" in
       let key2 = "albert.1_marie" in
       if Hashtbl.mem person_ref key1 || Hashtbl.mem person_ref key2 then
         Printf.eprintf "Albert in person_ref (2)\n"
       else Printf.eprintf "Albert not person_ref (2)\n");

    let old = List.length new_list in
    let new_list =
      let rec loop acc prev new_list =
        match new_list with
        | [] -> acc
        | ((_iper, sn, fn, oc, _aliases, kind, _key_l) as item) :: new_list ->
            let fn1, oc1, _sn1, sn2 =
              match kind with
              | "boat" -> (fn, oc, "X", "")
              | "list" -> (fn, oc, ".", "")
              | "alias" -> (fn, oc, sn, Sutil.particles sn)
              | _ -> (fn, oc, sn, Sutil.particles sn ^ ", ")
            in
            let key =
              Format.sprintf "%s%s%s" sn2 fn1
                (if oc1 <> 0 then Format.sprintf ".%d" oc1 else "")
            in
            if key <> prev then loop (item :: acc) key new_list
            else loop acc key new_list
      in
      loop [] "" (List.rev new_list)
    in
    if !verbose then
      Printf.eprintf "New list (duplicates) %d -> %d\n" old
        (List.length new_list);
    let och = open_out fname_out in
    print_index och base new_list person_ref letters;
    close_out och;
    Printf.eprintf "Done (%d)\n" (List.length new_list)
  with _ ->
    Printf.eprintf "Cannot open base %s\n" bname;
    exit 1

let () = try main () with e -> Printf.eprintf "%s\n" (Printexc.to_string e)
