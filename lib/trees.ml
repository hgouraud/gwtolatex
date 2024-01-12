(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)

let rule_thickns = "0.5pt"
let row_width row _n = List.fold_left (fun w (_, s, _, _, _, _) -> w + s) 0 row

let test_tree_width tree =
  let rec loop first i w0 tree =
    match tree with
    | [] -> (i, w0, w0, true)
    | row :: tree ->
        let w = row_width row i in
        if first then loop false (i + 1) w tree
        else if w <> w0 then (i + 1, w, w0, false)
        else loop false (i + 1) w0 tree
  in
  loop true 0 0 tree

(* no test on i relative to nb cols! *)
let is_empty_col row i =
  let rec loop j row =
    match row with
    | [] -> false
    | (_, s, ty, _, _, _) :: row ->
        if j + s < i then loop (j + s) row
        else
          let rec loop1 k =
            if j + k = i then ty = "E" || ty = "Hc" else loop1 (k + 1)
          in
          loop1 1
  in
  loop 0 row

(* an empty column is a column with only E and Hc -- *)
(* scan the whole tree ron by row to determine empty columns *)
let find_empty_columns tree =
  (* i is row number *)
  let rec loop first i cols tree =
    match tree with
    | [] -> cols
    | row :: tree ->
        if first then
          let cols, _ =
            List.fold_left
              (fun (acc, j) (_, s, ty, _, _, _) ->
                let cells =
                  (* j is column number, starts at 1 *)
                  let rec loop1 c_cols k =
                    let res =
                      (if ty = "E" || ty = "Hc" then "E" else "F")
                      ^ Format.sprintf "%d" (j + s - k)
                      (* k is counting down *)
                    in
                    if k = 0 then List.rev c_cols
                    else loop1 (res :: c_cols) (k - 1)
                  in
                  loop1 [] s
                in
                (List.rev cells :: acc, j + s))
              ([], 1) row
          in
          loop false (i + 1) (List.flatten cols |> List.rev) tree
        else
          let cols =
            List.mapi
              (fun i c ->
                if is_empty_col row (i + 1) then c
                else "F" ^ Format.sprintf "%d" (i + 1))
              cols
          in
          loop false (i + 1) cols tree
  in
  loop true 1 [] tree (* start at row 1 for numbering *)

let get_part lr side str =
  let str = Sutil.clean_double_back_slash str in
  let str = Sutil.suppress_trailing_sp str in
  let str = Sutil.suppress_leading_sp str in
  let str = Sutil.replace_utf8_bar str in
  let res =
    match (lr, side) with
    | 0, "left" -> String.sub str 1 (String.length str - 1)
    | 0, "right" -> String.sub str 1 (String.length str - 1)
    | 1, "left" -> String.sub str 0 (String.length str - 1)
    | 1, "right" -> String.sub str 0 (String.length str - 1)
    | _, _ -> str
  in
  res

let scan_row_for_bar row =
  let utf8_bar = String.of_seq (List.to_seq [ '\xE2'; '\x94'; '\x82' ]) in
  List.fold_left
    (fun (lr, bar) (_, _, _, te, it, _im) ->
      let i = try String.index te '|' with Not_found -> -1 in
      let j = try String.index it '|' with Not_found -> -1 in
      let k = try String.index te '\xE2' with Not_found -> -1 in
      let l = try String.index it '\xE2' with Not_found -> -1 in
      let left_right =
        if i = 0 || j = 0 || k = 0 || l = 0 then 0
        else if i = -1 && j = -1 && k = -1 && l = -1 then -1
        else 1
      in
      let bar =
        bar || String.contains te '|' || String.contains it '|'
        || Sutil.contains te utf8_bar || Sutil.contains it utf8_bar
      in
      let lr =
        match (lr, left_right) with
        | -1, 0 -> 0
        | 0, -1 -> 0
        | 0, 0 -> 0
        | -1, 1 -> 1
        | 1, -1 -> 1
        | 1, 1 -> 1
        | _, _ -> -1
      in
      (lr, bar))
    (-1, false) row

let copy_row row lr side =
  List.map
    (fun (w, s, ty, te, it, im) ->
      ( w,
        s,
        (if side = "bar" then match ty with "Te" | "It" -> "Hv2" | _ -> ty
        else ty),
        (if side = "bar" then ""
        else match ty with "Te" -> get_part lr side te | _ -> te),
        (if side = "bar" then ""
        else match ty with "It" -> get_part lr side it | _ -> it),
        if side = "bar" then ""
        else match ty with "Im" -> get_part lr side im | _ -> im ))
    row

let print_row row =
  Printf.eprintf "Row:\n";
  List.iter
    (fun (_, s, ty, te, it, im) ->
      let te = Sutil.clean_double_back_slash_2 te |> Sutil.clean_item in
      let it = Sutil.clean_double_back_slash_2 it |> Sutil.clean_item in
      Printf.eprintf "[ (%d) %s te:%s it:%s im:%s], " s ty te it im)
    row;
  Printf.eprintf "\n"

(* in fact, the extra | appears as :              *)
(* <a href ...>|</a><br><a href ...>content</a>   *)
let split_rows_with_vbar tree =
  List.fold_left
    (fun acc row ->
      let lr, bar = scan_row_for_bar row in
      (* lr = 0 -> | xxx, lr = 1 -> xxx |, lr = -1 -> xxx *)
      if bar then
        if lr = 0 then copy_row row lr "bar" :: copy_row row lr "right" :: acc
        else copy_row row lr "left" :: copy_row row lr "bar" :: acc
      else row :: acc)
    [] (List.rev tree)

let get_nb_full_col_in_span cols b s =
  let rec loop i n cols =
    match cols with
    | [] -> n
    | c :: cols ->
        if i < b then loop (i + 1) n cols
        else if i < b + s then
          if c.[0] = 'F' then loop (i + 1) (n + 1) cols else loop (i + 1) n cols
        else n
  in
  loop 0 0 cols

let get_item_length str =
  let len = String.length str in
  let rec loop i l1 l2 =
    if i = len then max l1 l2
    else if i < len - 1 && str.[i] = '\\' && str.[i + 1] = '\\' then
      loop (i + 2) 0 (max l1 l2)
    else loop (i + 1) (l1 + 1) l2
  in
  loop 0 0 0

(* find the largest item in a given column *)
let get_col_width _row _c = 1.0
let carwidth = 0.3

let reset_cell_width cols row textwidth imwidth =
  let nb_f_col =
    let rec loop i n cols =
      match cols with
      | [] -> n
      | c :: cols ->
          if c.[0] = 'F' then loop (i + 1) (n + 1) cols else loop (i + 1) n cols
    in
    loop 0 0 cols
  in
  let total_width =
    let rec loop i n cols =
      match row with
      | [] -> n
      | (_, _, ty, te, it, _im) :: row ->
          if ty = "It" || ty = "Te" || ty = "Im" then
            loop (i + 1)
              (n
              +. max imwidth
                   ((Float.of_int (get_item_length te) *. carwidth)
                   +. (Float.of_int (get_item_length it) *. carwidth)))
              cols
          else loop (i + 1) n row
    in
    loop 0 0. row
  in
  let unit_width = textwidth /. Float.of_int nb_f_col in
  let rec loop new_row c row =
    match row with
    | [] -> List.rev new_row
    | (_w, s, ty, te, it, im) :: row ->
        let width =
          (get_nb_full_col_in_span cols c s |> Float.of_int)
          *. unit_width *. get_col_width row c
          /. (* TODO ??? *)
          total_width
        in
        loop ((width, s, ty, te, it, im) :: new_row) (c + s) row
  in
  loop [] 0 row

(* <a href="base?m=IM&p=first_name&n=surname&occ=noc&k=first_name.noc.surname" *)
(* <a href="base?m=IM;s=test/filaname.jpg"> *)
(* ATTENTION assumes .jpg portrait file extension *)
(* TODO query the base for real extension ?? *)
let get_img_name base_name im =
  let _ext_l = [ ".jpg"; ".jpeg"; ".bnp" ] in
  let ext = ".jpg" in
  let where = "images" in
  (* or src *)
  let href_attrl = Hutil.split_href im in
  let k = Hutil.get_href_attr "k" href_attrl in
  let name =
    Format.sprintf "%s"
      (String.concat Filename.dir_sep [ "."; where; base_name; k ^ ext ])
  in
  Sutil.replace_str name "\\_{}" "_"

(*
  goal: split crammed rows in two
       |         |           |
   ----------    |      ----------
  |          |   |     |          |
   ----------    |      ----------
           --------------
          |              |
           --------------
- for each non empty column, add 2 extra cols (left and right)
- for col 1 and n add an extra col
- give to these cols the width txtw / nb_f_col / 3
- expand full colums to the right and left
- recompute colspan ??
*)

let expand_hrl_cells tree =
  let rec expand row new_row =
    match row with
    | (w1, s1, ty1, te1, it1, im1) :: row -> (
        match ty1 with
        | "Hl" when s1 > 1 ->
            let elts =
              if s1 / 2 * 2 = s1 then
                let rec loop acc s =
                  if s = 0 then acc
                  else if s <= s1 / 2 then
                    loop ([ (w1, 1, "E", "", "", "") ] @ acc) (s - 1)
                  else loop ([ (w1, 1, "Hc", "", "", "") ] @ acc) (s - 1)
                in
                loop [] s1
              else
                let rec loop acc s =
                  if s = 0 then acc
                  else if s < s1 / 2 then
                    loop ([ (w1, 1, "E", "", "", "") ] @ acc) (s - 1)
                  else if s = s1 / 2 then
                    loop ([ (w1, 1, "Hl", "", "", "") ] @ acc) (s - 1)
                  else loop ([ (w1, 1, "Hc", "", "", "") ] @ acc) (s - 1)
                in
                loop [] s1
            in
            expand row (elts @ new_row)
        | "Hr" when s1 > 1 ->
            let elts =
              if s1 / 2 * 2 = s1 then
                let rec loop acc s =
                  if s = 0 then acc
                  else if s <= s1 / 2 then
                    loop ([ (w1, 1, "Hc", "", "", "") ] @ acc) (s - 1)
                  else loop ([ (w1, 1, "E", "", "", "") ] @ acc) (s - 1)
                in
                loop [] s1
              else
                let rec loop acc s =
                  if s = 0 then acc
                  else if s < s1 / 2 then
                    loop ([ (w1, 1, "Hc", "", "", "") ] @ acc) (s - 1)
                  else if s = s1 / 2 then
                    loop ([ (w1, 1, "Hr", "", "", "") ] @ acc) (s - 1)
                  else loop ([ (w1, 1, "E", "", "", "") ] @ acc) (s - 1)
                in
                loop [] s1
            in
            expand row (elts @ new_row)
        | _ -> expand row ([ (w1, s1, ty1, te1, it1, im1) ] @ new_row))
    | _ -> List.rev (List.rev row @ new_row)
  in
  let tree =
    let rec loop tree new_tree =
      match tree with
      | [] -> new_tree
      | row :: tree -> loop tree (expand row [] :: new_tree)
    in
    loop tree []
  in
  List.rev tree

let expand_cells tree =
  let rec expand row new_row =
    match row with
    | (w1, s1, ty1, te1, it1, im1)
      :: (w2, s2, ty2, te2, it2, im2)
      :: (w3, s3, ty3, te3, it3, im3)
      :: row -> (
        match (ty1, ty2, ty3) with
        | ty1, ty2, ty3
          when ty1 = "E" && s1 = 1 && ty3 = "E" && s3 = 1 && ty2 <> "E"
               && (te2 <> "" || it2 <> "" || im2 <> "") ->
            expand row ([ (w2, s2 + 2, ty2, te2, it2, im2) ] @ new_row)
        | ty1, ty2, ty3
          when ty1 = "E" && s1 >= 2 && ty3 = "E" && s3 = 1 && ty2 <> "E"
               && (te2 <> "" || it2 <> "" || im2 <> "") ->
            expand row
              ([
                 (w1, s1 - 1, ty1, te1, it1, im1);
                 (w2, s2 + 2, ty2, te2, it2, im2);
               ]
              @ new_row)
        | ty1, ty2, ty3
          when ty1 = "E" && s1 = 1 && ty3 = "E" && s3 >= 2 && ty2 <> "E"
               && (te2 <> "" || it2 <> "" || im2 <> "") ->
            expand
              ([ (w3, s3 - 1, ty3, te3, it3, im3) ] @ row)
              ([ (w2, s2 + 2, ty2, te2, it2, im2) ] @ new_row)
        | ty1, ty2, ty3
          when ty1 = "E" && s1 >= 2 && ty3 = "E" && s3 >= 2 && ty2 <> "E"
               && (te2 <> "" || it2 <> "" || im2 <> "") ->
            expand
              ([ (w3, s3 - 1, ty3, te3, it3, im3) ] @ row)
              ([
                 (w2, s2 + 2, ty2, te2, it2, im2);
                 (w1, s1 - 1, ty1, te1, it1, im1);
               ]
              @ new_row)
        | ty1, ty2, ty3 ->
            expand
              ([ (w2, s2, ty2, te2, it2, im2); (w3, s3, ty3, te3, it3, im3) ]
              @ row)
              ([ (w1, s1, ty1, te1, it1, im1) ] @ new_row))
    | _ -> List.rev (List.rev row @ new_row)
  in
  let tree =
    let rec loop tree new_tree =
      match tree with
      | [] -> new_tree
      | row :: tree -> loop tree (expand row [] :: new_tree)
    in
    loop tree []
  in
  List.rev tree

let print_tree base_name tree mode textwidth textheight _margin debug fontsize
    sideways imgwidth twopages =
  if debug <> 0 then
    Printf.eprintf "Print Tree g=%d, depth=%d\n" mode (List.length tree);
  let i, w, w0, ok = test_tree_width tree in
  if not ok then (
    Printf.eprintf "Unbalanced tree, row %d w=%d, w0=%d\n" i w w0;
    exit 1);
  if mode = 1 then
    let _nb_col = List.length (List.nth tree 0) in
    let cols = find_empty_columns tree in
    (*let tree = split_rows_with_vbar tree in*)
    let tree = expand_cells tree in
    let tree = expand_hrl_cells tree in
    let non_empty_col_nbr =
      List.fold_left (fun a c -> if c.[0] = 'F' then a + 1 else a) 0 cols
    in
    let tabular_env =
      let rec loop s i = if i = 0 then s ^ "c" else loop (s ^ "c") (i - 1) in
      loop "ccccc" (List.length cols)
    in
    let cell_wid =
      ((if sideways then textheight *. if twopages then 2.0 else 1.0
       else textwidth)
      -. 4.0)
      /. Float.of_int non_empty_col_nbr
    in
    let half_cell_wid = cell_wid /. 2.0 in
    (* on top of tabular, one can use tables
       \begin{table}[h!]
       \centering
       \rotatebox{90}{
         \begin{tabular}{||c c c c||}
           ......
         \end{tabular}
       }
       \caption{Table to test captions and labels.}
       \label{table:1}
       \end{table}
    *)
    let tabular_b =
      Format.sprintf "%s\n\\begin{tabular}{%s}\n"
        (if sideways then "\\begin{sideways}" else "")
        tabular_env
    in
    let tabular_e =
      Format.sprintf "\\end{tabular}\n%s"
        (if sideways then "\\end{sideways}\n" else "")
    in
    tabular_b
    ^ List.fold_left
        (fun acc1 row ->
          (* print_row row; *)
          let _, row_str =
            List.fold_left
              (fun (c, acc2) (_, s, ty, te, it, im) ->
                let colspan_b =
                  if s > 1 then Format.sprintf "\\multicolumn{%d}{c}{" s else ""
                in
                let colspan_e = if s > 1 then "}" else "" in
                (* compute new_wid taking into account empty columns *)
                let nb_full_col = get_nb_full_col_in_span cols c s in
                let new_wid = cell_wid *. Float.of_int nb_full_col in
                (* for testing purposes, add \\fbox{ to minipage_b and } to minipage_e *)
                let fbox_b = if debug = 1 then "\\fbox{" else "" in
                let fbox_e = if debug = 1 then "}" else "" in
                let minipage_b =
                  Format.sprintf "%s\\begin{minipage}{%1.2fcm}" fbox_b new_wid
                in
                let minipage_e = Format.sprintf "\\end{minipage}%s" fbox_e in
                let font_b =
                  if fontsize = "" then "" else "\\" ^ fontsize ^ "{"
                in
                let font_e = if fontsize = "" then "" else "}" in
                (* parbox width and multicolums clash *)
                let cell_str =
                  Format.sprintf "%s%s" colspan_b minipage_b
                  (* begin of cell *)
                  ^ (match ty with
                    | "Te" | "It" -> (
                        let te =
                          Sutil.replace '\n' ' ' te |> Sutil.suppress_leading_sp
                          |> Sutil.clean_double_back_slash_2
                          |> Sutil.clean_leading_double_back_slash
                          |> Sutil.clean_item
                        in
                        let it =
                          Sutil.replace '\n' ' ' it |> Sutil.suppress_leading_sp
                          |> Sutil.clean_double_back_slash_2
                          |> Sutil.clean_leading_double_back_slash
                          |> Sutil.clean_item
                        in
                        match (te, it) with
                        | "", it when it <> "" ->
                            "\\begin{center}" ^ font_b ^ it ^ font_e
                            ^ "\\end{center}"
                        | te, "" when te <> "" ->
                            "\\begin{center}" ^ font_b ^ te ^ font_e
                            ^ "\\end{center}"
                        | te, it when te <> "" && it <> "" ->
                            "\\begin{center}" ^ font_b ^ te ^ "\\\\" ^ it
                            ^ font_e ^ "\\end{center}"
                        | "", "" -> ""
                        | _, _ ->
                            "\\begin{center}" ^ font_b ^ te ^ it ^ font_e
                            ^ "\\end{center}")
                    | "Hl" ->
                        Format.sprintf
                          "\\begin{flushleft}\\rule{%1.2fcm}{%s}\\end{flushleft}"
                          half_cell_wid rule_thickns
                    | "Hr" ->
                        Format.sprintf
                          "\\begin{flushright}\\rule{%1.2fcm}{%s}\\end{flushright}"
                          half_cell_wid rule_thickns
                    | "Hc" ->
                        Format.sprintf
                          "\\begin{center}\\rule{%1.2fcm}{%s}\\end{center}"
                          new_wid rule_thickns
                    | "Hv1" ->
                        Format.sprintf
                          "\\begin{center}\\rule{%s}{%s}\\end{center}"
                          rule_thickns "0.5cm"
                    | "Hv2" ->
                        Format.sprintf
                          "\\begin{center}\\rule{%s}{%s}\\end{center}"
                          rule_thickns "0.5cm"
                    | "E" -> ""
                    | "Im" ->
                        Format.sprintf
                          {|\\begin{center}
                          \\includegraphics[width=%1.2fcm]{%s}
                          \\end{center}|}
                          imgwidth
                          (get_img_name base_name im)
                    | _ -> "??")
                  ^ Format.sprintf "%s%s" minipage_e colspan_e
                  (* end of cell *)
                in
                (c + s, acc2 ^ cell_str ^ " &\n"))
              (0, "") row
          in
          (* Printf.eprintf "Row string: %s\n" row_str;*)
          acc1 ^ row_str ^ "\\\\\n")
        "" tree
    ^ tabular_e
  else
    (*let tree = split_rows_with_vbar tree in*)
    let tree = expand_cells tree in
    let tree = expand_hrl_cells tree in
    let tree, _n =
      List.fold_left
        (fun (acc1, r) row ->
          let str =
            List.fold_left
              (fun acc2 (_, s, ty, te, it, im) ->
                let cell =
                  (match ty with
                  | "Te" -> "Te " ^ Sutil.clean_double_back_slash te
                  | "It" -> "It " ^ Sutil.clean_double_back_slash it
                  | "Hl" -> "Hr " ^ "-l"
                  | "Hr" -> "Hr " ^ "r-"
                  | "Hc" -> "Hr " ^ "--"
                  | "Hv1" -> "Vr1 " ^ "|"
                  | "Hv2" -> "Vr2 " ^ "|"
                  | "E" -> "E" ^ ""
                  | "Im" -> "Im " ^ im
                  | _ -> "x")
                  ^ if s > 1 then Format.sprintf "(%d)" s else ""
                in
                acc2 ^ "[" ^ cell ^ "] ")
              "" row
          in
          ( acc1 ^ Format.sprintf "Row %d: (%d) %s\\\\\n" r (List.length row) str,
            r + 1 ))
        ("", 1) tree
    in
    Format.sprintf "Interim print (%d)\\\\\n %s\n" (String.length tree) tree
