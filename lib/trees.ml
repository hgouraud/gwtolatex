(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)

let rule_thickns = "0.5pt"
let row_width row _n = List.fold_left (fun w (_, s, _, _, _) -> w + s) 0 row

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
    | (_, s, ty, _, _) :: row ->
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
              (fun (acc, j) (_, s, ty, _, _) ->
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
    (fun (lr, bar) (_, _, _, te, it) ->
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
    (fun (w, s, ty, te, it) ->
      ( w,
        s,
        (if side = "bar" then match ty with "Te" | "It" -> "Hv2" | _ -> ty
        else ty),
        (if side = "bar" then ""
        else match ty with "Te" -> get_part lr side te | _ -> te),
        if side = "bar" then ""
        else match ty with "It" -> get_part lr side it | _ -> it ))
    row

let print_row row =
  Printf.eprintf "Row:\n";
  List.iter
    (fun (_, s, ty, te, it) ->
      let te = Sutil.clean_double_back_slash_2 te |> Sutil.clean_item in
      let it = Sutil.clean_double_back_slash_2 it |> Sutil.clean_item in
      Printf.eprintf "[ (%d) %s te:%s it:%s ], " s ty te it)
    row;
  Printf.eprintf "\n"

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

let get_nb_full_col cols b s =
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

let print_tree tree mode textwidth textheight _margin debug fontsize sideways =
  if debug <> 0 then Printf.eprintf "Print Tree mode=%d\n" mode;
  let i, w, w0, ok = test_tree_width tree in
  if not ok then (
    Printf.eprintf "Unbalanced tree, row %d w=%d, w0=%d\n" i w w0;
    exit 1);
  if mode = 1 then
    let cols = find_empty_columns tree in
    let tree = split_rows_with_vbar tree in
    let non_empty_col_nbr =
      List.fold_left (fun a c -> if c.[0] = 'F' then a + 1 else a) 0 cols
    in
    let tabular_env =
      let rec loop s i = if i = 0 then s ^ "c" else loop (s ^ "c") (i - 1) in
      loop "ccccc" non_empty_col_nbr
    in
    let cell_wid =
      (if sideways then textheight else textwidth)
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
              (fun (c, acc2) (_, s, ty, te, it) ->
                let colspan_b =
                  if s > 1 then Format.sprintf "\\multicolumn{%d}{c}{" s else ""
                in
                let colspan_e = if s > 1 then "}" else "" in
                (* compute new_wid taking into account empty columns *)
                let nb_full_col = get_nb_full_col cols c s in
                let new_wid = cell_wid *. Float.of_int nb_full_col in
                (* for testing purposes, add \\fbox{ to parbox_b and } to parbox_e *)
                let minipage_b =
                  Format.sprintf "\\fbox{\\begin{minipage}{%1.2fcm}" new_wid
                in
                let minipage_e = Format.sprintf "\\end{minipage}}" in
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
                    | "Im" -> "Image"
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
    let tree = split_rows_with_vbar tree in
    let tree, _n =
      List.fold_left
        (fun (acc1, r) row ->
          let str =
            List.fold_left
              (fun acc2 (_, s, ty, te, it) ->
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
                  | "Im" -> "Im " ^ "Image"
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
