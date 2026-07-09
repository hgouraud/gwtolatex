(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)
(* v2  Henri+Claude, 2026/05 — dynamic Vr heights via row classification *)
open TreeAux

let row_width row = List.fold_left (fun w (_, s, _, _, _, _) -> w + s) 0 row
let row_nb = ref 0
let nb_head_rows = ref 0

(* ── Row array for nearest_sig scanning ─────────────────────────── *)
(* convert tree list to array once per print call so nearest_sig
   can index in O(1)                                                 *)
let tree_to_array tree = Array.of_list tree

let print_tree (conf : Config.config) tree =
  let i, w, w0, ok = test_tree_width tree 0 in
  if not ok then (
    Printf.eprintf "Unbalanced tree, row %d w=%d, w0=%d\n" i w w0;
    exit 1);
  let nb_head_rows = get_nb_head_rows tree in

  let init_cols tree nb_head_rows =
    let cols = find_empty_columns tree nb_head_rows in
    let col_f_n =
      List.fold_left (fun a col -> if col.[0] = 'F' then a + 1 else a) 0 cols
    in
    let tree_width =
      if conf.sideways then conf.textheight else conf.textwidth
    in
    let col_sep = conf.colsep in
    let col_e_w = conf.colsep in
    let col_n = List.length cols in
    let col_e_n = col_n - col_f_n in
    let col_f_w =
      (tree_width
      -. (Float.of_int (col_n - 1) *. col_sep)
      -. (Float.of_int col_e_n *. col_e_w))
      /. Float.of_int col_f_n
    in
    let colwidth = col_f_w in
    let tabular_env =
      let colspec_f = Format.sprintf "P{%1.2fcm}" col_f_w in
      let colspec_e = Format.sprintf "P{%1.2fcm}" col_e_w in
      let empty_col i = (List.nth cols i).[0] = 'E' in
      let tab_env =
        let rec loop res i =
          if i = col_n then res
          else
            loop ((if empty_col i then colspec_e else colspec_f) :: res) (i + 1)
        in
        loop [] 0 |> List.rev
      in
      let tab_env =
        if conf.debug = 1 then "|" ^ String.concat "|" tab_env ^ "|c|"
        else String.concat "" tab_env
      in
      tab_env ^ "c"
    in
    (cols, tabular_env, colwidth)
  in

  (* ── Mode 1: actual LaTeX tabular output ─────────────────────── *)
  let print_tree_mode_1 (conf : Config.config) tree page =
    let tree =
      let rec loop n tree =
        match n with 0 -> tree | _ -> loop (n - 1) (expand_cells conf tree)
      in
      loop conf.expand tree
    in
    let tree = squeeze_row_tree tree in
    let tree = remove_duplicate_rows tree in
    let cols, tabular_env, colwidth = init_cols tree nb_head_rows in
    let cols_str, tab_env = print_tab_env cols tabular_env in
    if conf.debug = 1 then
      Format.eprintf "Tabular env: tree length: %d\n%s\n%s\n" (List.length tree)
        cols_str tab_env;

    (* Build row array for nearest_sig lookups *)
    let rows = tree_to_array tree in

    let offset_b =
      if conf.hoffset <> 0. then
        Format.sprintf "\\hspace*{%1.2f%s}\n" conf.hoffset conf.unit
      else ""
    in
    let tabular_b =
      Format.sprintf
        "%s\\nohyphens\\newcolumntype{P}[1]{>{\\centering\\arraybackslash}p{#1}}\n\
         \\renewcommand*{\\arraystretch}{0.1}\\renewcommand*{\\tabcolsep}{%1.2f%s}%s\\begin{tabular}{%s}\n"
        (if conf.sideways then "\\begin{sideways}" else "")
        conf.colsep conf.unit offset_b tabular_env
    in
    let tabular_e =
      Format.sprintf "\\end{tabular}%s\n\\hyphenation{nor-mal-ly}\n"
        (if conf.sideways then "\\end{sideways}\n" else "")
    in

    row_nb := 0;
    tabular_b
    ^ List.fold_left
        (fun acc1 row ->
          let ri = !row_nb in
          incr row_nb;
          let row = List.rev row in
          let _, row_str =
            List.fold_left
              (fun (col, acc2) (_, s, ty, te, it, im) ->
                if s = 0 then (col, acc2)
                else
                  let fbox_b = if conf.debug = 999 then "\\fbox{" else "" in
                  let fbox_e = if conf.debug = 999 then "}" else "" in
                  let minipage_b =
                    Format.sprintf "%s\\begin{minipage}{%1.2f%s}\\begin{center}"
                      fbox_b
                      (colwidth *. Float.of_int s)
                      conf.unit
                  in
                  let minipage_e =
                    Format.sprintf "\\end{center}\\end{minipage}%s" fbox_e
                  in
                  let font_b =
                    if conf.fontsize = "" then ""
                    else "\\" ^ conf.fontsize ^ "{"
                  in
                  let font_e = if conf.fontsize = "" then "" else "}" in

                  (* ── Horizontal rule helper ── *)
                  let hr s lrc =
                    let rec loop i acc =
                      if i = s then acc
                      else
                        loop (i + 1)
                          (acc
                          ^ (if lrc = "e" then ""
                             else
                               Format.sprintf
                                 "{\\centering %s\\rule[0pt]{%s}{%1.2fpt}}"
                                 (if lrc = "r" then
                                    Format.sprintf "\\hspace{%1.2f%s}"
                                      (colwidth /. 4.0) conf.unit
                                  else if lrc = "l" then
                                    Format.sprintf "\\hspace{-%1.2f%s}"
                                      (colwidth /. 4.0) conf.unit
                                  else "")
                                 (if lrc = "c" then
                                    Format.sprintf "%1.2f%s" colwidth conf.unit
                                  else
                                    Format.sprintf "%1.2f%s" (colwidth /. 2.0)
                                      conf.unit)
                                 conf.rulethickns)
                          ^ if i + 1 = s then "" else "&")
                    in
                    loop 0 ""
                  in

                  (* ── Vertical rule with dynamic height ─────────
                     Use nearest_sig to determine what this bar row
                     connects: content above, branch below, etc.
                     This mirrors dagSvg.js bar endpoint logic.     *)
                  let vr_rule is_short =
                    let h_cm, short =
                      if is_short then (conf.rulethickns /. 10.0, true)
                      else vr_height_cm conf rows ri
                    in
                    let h_cm, short =
                      if is_short then (h_cm, true) else (h_cm, short)
                    in
                    if short then
                      (* Vr2: small square dot *)
                      Format.sprintf "\\rule[0pt]{%1.2fpt}{%1.2fpt}"
                        conf.rulethickns conf.rulethickns
                    else
                      (* Vr1: full-height rule *)
                      Format.sprintf "\\rule[0pt]{%1.2fpt}{%1.2fcm}"
                        conf.rulethickns h_cm
                  in

                  let cell_str =
                    if (List.nth cols col).[0] = 'E' then ""
                    else
                      match ty with
                      | "Te" | "It" ->
                          let str =
                            let te =
                              Sutil.replace '\n' ' ' te
                              |> Sutil.suppress_leading_sp
                              |> Sutil.clean_double_back_slash_2
                              |> Sutil.clean_leading_double_back_slash
                              |> Sutil.clean_item
                            in
                            let it =
                              Sutil.replace '\n' ' ' it
                              |> Sutil.suppress_leading_sp
                              |> Sutil.clean_double_back_slash_2
                              |> Sutil.clean_leading_double_back_slash
                              |> Sutil.clean_item
                            in
                            match (te, it) with
                            | "", it when it <> "" -> font_b ^ it ^ font_e
                            | te, "" when te <> "" -> font_b ^ te ^ font_e
                            | te, it when te <> "" && it <> "" ->
                                font_b ^ te ^ "\\\\" ^ it ^ font_e
                            | "", "" -> ""
                            | _, _ -> font_b ^ te ^ it ^ font_e
                          in
                          if s = 1 then
                            Format.sprintf "%s%s%s" minipage_b str minipage_e
                          else
                            Format.sprintf "\\multicolumn{%d}{c}{%s%s%s}" s
                              minipage_b str minipage_e
                      | "Hl" ->
                          let odd = s / 2 * 2 <> s in
                          if s = 1 then hr s "l"
                          else if odd then
                            hr (s / 2) "c"
                            ^ "&\n" ^ hr 1 "l" ^ " &\n"
                            ^ hr (s / 2) "e"
                          else hr (s / 2) "c" ^ "&\n" ^ hr (s / 2) "e"
                      | "Hr" ->
                          let odd = s / 2 * 2 <> s in
                          if s = 1 then hr s "r"
                          else if odd then
                            hr (s / 2) "e"
                            ^ "&\n" ^ hr 1 "r" ^ "&\n"
                            ^ hr (s / 2) "c"
                          else hr (s / 2) "e" ^ "&\n" ^ hr (s / 2) "c"
                      | "Hc" ->
                          let rec loop i acc =
                            if i = s then acc
                            else
                              loop (i + 1)
                                (acc
                                ^ Format.sprintf
                                    "\\rule[0pt]{%1.2f%s}{%1.2fpt}%s" colwidth
                                    conf.unit conf.rulethickns
                                    (if i + 1 = s then "" else "&\n"))
                          in
                          loop 0 ""
                      | "Vr1" ->
                          let rule = vr_rule false in
                          if s = 1 then rule
                          else Format.sprintf "\\multicolumn{%d}{c}{%s}" s rule
                      | "Vr2" ->
                          let rule = vr_rule true in
                          if s = 1 then rule
                          else Format.sprintf "\\multicolumn{%d}{c}{%s}" s rule
                      | "E" ->
                          if s = 1 then ""
                          else Format.sprintf "\\multicolumn{%d}{c}{}" s
                      | "Im" ->
                          Format.sprintf
                            {|%s\\includegraphics[width=%1.2fcm]{%s}%s|}
                            minipage_b conf.imgwidth
                            (get_img_name conf.basename im)
                            minipage_e
                      | _ -> "??"
                  in
                  (col + s, cell_str :: acc2))
              (0, []) row
          in
          let row_str = String.concat "&" row_str in
          (* Two-page linking arrows *)
          let row_str =
            if conf.twopages && page = "left" && !row_nb = nb_head_rows + 1 then
              row_str ^ "$\\hspace{-0.5cm}\\rightarrow$"
            else row_str
          in
          let row_str =
            if conf.twopages && page = "right" && !row_nb = nb_head_rows + 1
            then "$\\leftarrow\\hspace{-0.5cm}$" ^ row_str
            else row_str
          in
          acc1 ^ row_str ^ "\\\\\n")
        "" tree
    ^ tabular_e
  in

  (* ── Mode 0: debug text dump ─────────────────────────────────── *)
  let print_tree_mode_0 _conf tree =
    let cols, tabular_env, _colwidth = init_cols tree nb_head_rows in
    let tree, _n =
      List.fold_left
        (fun (acc1, r) row ->
          let row = List.rev row in
          let span_t =
            List.fold_left (fun acc (_, s, _, _, _, _) -> acc + s) 0 row
          in
          let _, j, str =
            List.fold_left
              (fun (i, j, acc2) (_, s, ty, te, it, im) ->
                let cell =
                  (match ty with
                    | "Te" -> "Te " ^ Sutil.clean_double_back_slash te
                    | "It" -> "It " ^ Sutil.clean_double_back_slash it
                    | "Hl" -> "Hr " ^ "-l"
                    | "Hr" -> "Hr " ^ "r-"
                    | "Hc" -> "Hr " ^ "--"
                    | "Vr1" -> "Vr1 " ^ "|"
                    | "Vr2" -> "Vr2 " ^ "|"
                    | "E" -> "E"
                    | "Im" -> "Im " ^ im
                    | _ -> "x")
                  ^ Format.sprintf "(%d)" s
                in
                (i + 1, j + s, acc2 ^ Format.sprintf "[(%d)" i ^ cell ^ "] "))
              (0, 0, "") row
          in
          ( acc1
            ^ Format.sprintf "Row %d: (%d) (%d)%s(%d)\\\\\n" r (List.length row)
                span_t str j,
            r + 1 ))
        ("", 1) tree
    in
    let cols_str, tab_env = print_tab_env cols tabular_env in
    Format.sprintf "Interim print (%d)\\\\\n %s\\par\n%s\\par\n%s\n"
      (String.length tree) cols_str tab_env tree
  in

  (* ── Pipeline ────────────────────────────────────────────────── *)
  let tree = flip_tree_h tree in
  test_zero_span_t tree "init";
  let tree = remove_empty_cols conf tree nb_head_rows in
  let i, w, w0, ok = test_tree_width tree nb_head_rows in
  if not ok then (
    Printf.eprintf "Unbalanced tree, row %d w=%d, w0=%d\n" i w w0;
    exit 1);
  test_zero_span_t tree "after empty cols";

  if conf.twopages then (
    let tree_left, tree_right = split_tree conf tree in
    test_zero_span_t tree_left "tree_left";
    test_zero_span_t tree_right "tree right";
    match conf.treemode with
    | 0 -> print_tree_mode_0 conf tree_right ^ print_tree_mode_0 conf tree_left
    | 1 ->
        (if conf.debug = 1 then print_tree_mode_0 conf tree_left ^ "\\newpage"
         else "")
        ^ print_tree_mode_1 conf tree_right "right"
        ^ (if conf.debug = 1 then
             print_tree_mode_0 conf tree_right ^ "\\newpage"
           else if conf.samepage then "\\hspace{40mm}\n"
           else "\\vskip 20mm")
        ^ print_tree_mode_1 conf tree_left "left"
    | n -> Printf.sprintf "Error: bad tree mode %d\n" n)
  else
    match conf.treemode with
    | 0 -> print_tree_mode_0 conf tree
    | 1 ->
        (if conf.debug = 1 then print_tree_mode_0 conf tree ^ "\\newpage"
         else "")
        ^ print_tree_mode_1 conf tree ""
    | n -> Printf.sprintf "Error: bad tree mode %d\n" n
