(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)
(* v2  Henri+Claude, 2026/05 — dynamic Vr heights, row classification *)

let row_nb = ref 0
let nb_head_rows = ref 0

type im_type = Portrait | Imagek | Images | Vignette

type image = {
  im_type : im_type;
  filename : string;
  where : int * int * int * int;
  image_nbr : int;
}

(* width, span, (E Hr Hc Hl Vr1 Vr2 Te It Im), item, text, image *)
type c_type = E | Hc | Hr | Hl | Vr1 | Vr2 | Te | It | Im

type t_cell = {
  width : float;
  span : int;
  typ : string;
  item : string;
  txt : string;
  img : int;
}

type t_line = t_cell list
type t_table = { row : int; col : int; body : t_line list }

(* ── Row classification ─────────────────────────────────────────────
   Mirrors dagSvg.js classifyRow():
     content  — row has Te/It/Im cells (person nodes)
     bar      — row has Vr1/Vr2 cells (vertical connectors)
     branch   — row has Hr/Hl cells (horizontal branch spread)
     sibling  — row has only Hc cells (dashed sibling reach)
     empty    — row has only E cells
   A row may be both 'bar' and 'sibling' (mixed); classification
   returns the dominant type for nearestSig scanning.              *)
type row_kind = Content | Bar | Branch | Sibling | Empty

let classify_row row =
  let has_content =
    List.exists
      (fun (_, _, ty, _, _, _) -> ty = "Te" || ty = "It" || ty = "Im")
      row
  in
  let has_bar =
    List.exists (fun (_, _, ty, _, _, _) -> ty = "Vr1" || ty = "Vr2") row
  in
  let has_branch =
    List.exists (fun (_, _, ty, _, _, _) -> ty = "Hr" || ty = "Hl") row
  in
  let has_hr = List.exists (fun (_, _, ty, _, _, _) -> ty = "Hc") row in
  if has_content then Content
  else if has_bar then Bar
  else if has_branch then Branch
  else if has_hr then Sibling
  else Empty

(* ── Nearest significant row in direction ────────────────────────── *)
(* Scan from index ri in direction dir (+1 or -1).
   Skips Empty and Bar rows, returns the first Content/Branch/Sibling. *)
let nearest_sig rows ri dir =
  let n = Array.length rows in
  let rec loop r =
    if r < 0 || r >= n then None
    else
      match classify_row rows.(r) with
      | Content | Branch | Sibling -> Some (r, classify_row rows.(r))
      | Bar | Empty -> loop (r + dir)
  in
  loop (ri + dir)

(* ── Content height estimation ───────────────────────────────────── *)
(* Estimate the rendered height of a Te/It/Im cell in cm.
   Used to make Vr1 rules span exactly the gap between content cells,
   mirroring dagSvg.js's contentBottom/contentTop snapping.
   This is necessarily approximate for LaTeX without a second pass.    *)
let estimate_content_height_cm (conf : Config.config) (_, _, ty, te, it, _im) =
  match ty with
  | "Im" -> conf.imgwidth (* image is square-ish by default *)
  | "Te" | "It" ->
      let text = if ty = "Te" then te else it in
      (* rough estimate: each line ~0.4cm at \small, ~0.5cm at \normalsize *)
      let line_h = 0.45 in
      (* count approximate lines from string length and column width *)
      let chars_per_line = int_of_float (conf.textwidth /. 0.22) in
      (* ~2.2mm per char *)
      let nlines = max 1 ((String.length text / max 1 chars_per_line) + 1) in
      Float.of_int nlines *. line_h
  | _ -> 0.0

(* For a bar row at index ri in rows[], compute the vertical rule height (cm)
   needed to fill the gap between adjacent content rows.
   Rule:
     - if above is Branch/Sibling → use standard inter-branch gap
     - if above is Content → from contentBottom (≈ content_h) to ...
     - if below is Branch/Sibling → end at branch row
     - if below is Content → end at contentTop (≈ 0.0 from top of cell)
   Returns (h_cm, is_short) where is_short marks Vr2-style dots.      *)
let vr_height_cm (conf : Config.config) rows ri =
  let above = nearest_sig rows ri (-1) in
  let below = nearest_sig rows ri 1 in
  let branch_gap = 0.3 in
  (* cm between branch line and content *)
  let node_to_branch =
    (* height from contentBottom of node above down to branch line *)
    match above with
    | Some (ai, Content) ->
        (* find the content cell in row ai — use max height in row *)
        let max_h =
          List.fold_left
            (fun h cell -> max h (estimate_content_height_cm conf cell))
            0.0 rows.(ai)
        in
        (* gap = row height - content height + half branch row height *)
        max_h +. branch_gap
    | Some (_, Branch) | Some (_, Sibling) -> branch_gap
    | _ -> branch_gap
  in
  let branch_to_node =
    match below with
    | Some (bi, Content) ->
        let max_h =
          List.fold_left
            (fun h cell -> max h (estimate_content_height_cm conf cell))
            0.0 rows.(bi)
        in
        max_h +. branch_gap
    | Some (_, Branch) | Some (_, Sibling) -> branch_gap
    | _ -> branch_gap
  in
  match (above, below) with
  | Some (_, Content), Some (_, Content) ->
      (* direct bar between two content rows — medium height *)
      let h = node_to_branch +. branch_to_node in
      (h, false)
  | Some (_, Content), Some (_, (Branch | Sibling)) ->
      (* parent bar — from content down to branch line *)
      (node_to_branch, false)
  | Some (_, (Branch | Sibling)), Some (_, Content) ->
      (* child bar — from branch line down to content *)
      (branch_to_node, false)
  | Some (_, (Branch | Sibling)), Some (_, (Branch | Sibling)) ->
      (* bar between two branch rows — short dot *)
      (branch_gap, true)
  | _ -> (0.2, false)
(* fallback *)

let row_width row = List.fold_left (fun w (_, s, _, _, _, _) -> w + s) 0 row

let nbr_empty_cols cols =
  List.fold_left (fun n col -> if col.[0] = 'E' then n + 1 else n) 0 cols

let print_row row n =
  Printf.eprintf "Row %d = \n" n;
  List.iter
    (fun (_, s, ty, te, it, im) ->
      let te = Sutil.clean_double_back_slash_2 te |> Sutil.clean_item in
      let it = Sutil.clean_double_back_slash_2 it |> Sutil.clean_item in
      Printf.eprintf "[ (%d) %s te:%s it:%s im:%s], " s ty te it im)
    row;
  Printf.eprintf "\n"

let row_span _cols row _n =
  List.fold_left (fun span (_, s, _, _, _, _) -> span + s) 0 row

let print_row_span cols row n =
  Printf.eprintf "Row %d: %d cells, span %d\n" n (List.length row)
    (row_span cols row n)

let print_tree_rows_span cols tree =
  let rec loop i = function
    | [] -> ()
    | row :: tree ->
        print_row_span cols row i;
        loop (i + 1) tree
  in
  loop 0 tree

let print_tree tree n =
  Printf.eprintf "print tree (%d)\n" n;
  let rec loop i = function
    | [] -> ()
    | row :: tree when i < n ->
        print_row row i;
        loop (i + 1) tree
    | _ -> ()
  in
  loop 0 tree

let test_tree_width tree nb_head_rows =
  let rec loop first i w0 = function
    | [] -> (i, w0, w0, true)
    | row :: tree when i >= nb_head_rows ->
        let w = row_width row in
        if first then loop false (i + 1) w tree
        else if w <> w0 then (i + 1, w, w0, false)
        else loop false (i + 1) w0 tree
    | _row :: tree -> loop true (i + 1) w0 tree
  in
  loop true 0 0 tree

let is_empty_cell row i =
  let rec loop j = function
    | [] -> false
    | (_, s, ty, _, _, _) :: row ->
        if j + s < i then loop (j + s) row
        else
          let rec loop1 k =
            if j + k = i then
              ty = "E" || ty = "Hc" || ty = "Hr --" || ty = "Hr r-"
              || ty = "Hr -l"
            else loop1 (k + 1)
          in
          loop1 1
  in
  loop 0 row

let update_cols old_cols row _row_nb =
  let rec loop cols i = function
    | [] -> List.rev cols
    | (_, s, ty, _, _, _) :: row ->
        let cols1 =
          let rec loop1 cols1 j =
            let j1 = string_of_int (i + j) in
            if j = s then cols1
            else if (List.nth old_cols (i + j)).[0] <> 'E' then
              loop1 (("F" ^ j1) :: cols1) (j + 1)
            else if ty = "E" || ty = "Hc" || ty = "Hr --" then
              loop1 (("E" ^ j1) :: cols1) (j + 1)
            else loop1 (("F" ^ j1) :: cols1) (j + 1)
          in
          loop1 [] 0
        in
        loop (cols1 @ cols) (i + s) row
  in
  loop [] 0 row

let find_empty_columns tree nb_head_rows =
  let _i, width, _w0, ok = test_tree_width tree nb_head_rows in
  let cols =
    if ok then
      let rec loop n acc = if n = 0 then acc else loop (n - 1) ("E" :: acc) in
      loop width []
    else (
      Printf.eprintf "Unbalanced tree (find empty cols)\n";
      [])
  in
  let rec loop i cols = function
    | [] -> cols
    | row :: tree when i >= nb_head_rows ->
        loop (i + 1) (update_cols cols row i) tree
    | _row :: tree -> loop (i + 1) cols tree
  in
  loop 0 cols tree

let remove_empty_cells cols row _n =
  let rec loop i new_row = function
    | [] -> List.rev new_row
    | (a, s, typ, c, d, e) :: row ->
        if s = 1 then
          if (List.nth cols i).[0] = 'E' then loop (i + 1) new_row row
          else loop (i + 1) ((a, s, typ, c, d, e) :: new_row) row
        else
          let p =
            let rec loop1 p si =
              if si = s then p
              else if (List.nth cols (i + si)).[0] = 'E' then
                loop1 (p + 1) (si + 1)
              else loop1 p (si + 1)
            in
            loop1 0 0
          in
          let new_s = s - p in
          if new_s <= 0 then loop (i + s) new_row row
          else loop (i + s) ((a, new_s, typ, c, d, e) :: new_row) row
  in
  loop 0 [] row

let remove_empty_cols _conf tree nb_head_rows =
  let cols = find_empty_columns tree nb_head_rows in
  let tree2 =
    let rec loop i new_tree = function
      | [] -> List.rev new_tree
      | row :: tree when i >= nb_head_rows ->
          loop (i + 1) (remove_empty_cells cols row i :: new_tree) tree
      | row :: tree -> loop (i + 1) (row :: new_tree) tree
    in
    loop 0 [] tree
  in
  let cols = find_empty_columns tree2 nb_head_rows in
  let new_span = List.length cols in
  let rec loop i new_tree = function
    | [] -> List.rev new_tree
    | [ (w, _s, ty, te, it, im) ] :: tree when i < nb_head_rows ->
        loop (i + 1) ([ (w, new_span, ty, te, it, im) ] :: new_tree) tree
    | row :: tree -> loop (i + 1) (row :: new_tree) tree
  in
  loop 0 [] tree2

let get_part lr side str =
  let str = Sutil.clean_double_back_slash str in
  let str = Sutil.suppress_trailing_sp str in
  let str = Sutil.suppress_leading_sp str in
  let str = Sutil.replace_utf8_bar str in
  let len = String.length str in
  match (lr, side) with
  | 0, ("left" | "right") when len > 1 -> String.sub str 1 (len - 1)
  | 1, ("left" | "right") when len > 1 -> String.sub str 0 (len - 1)
  | _, _ -> str

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
        | -1, 0 | 0, -1 | 0, 0 -> 0
        | -1, 1 | 1, -1 | 1, 1 -> 1
        | _, _ -> -1
      in
      (lr, bar))
    (-1, false) row

let copy_row row lr side =
  List.map
    (fun (w, s, ty, te, it, im) ->
      ( w,
        s,
        (if side = "bar" then match ty with "Te" | "It" -> "Vr1" | _ -> ty
         else ty),
        (if side = "bar" then ""
         else match ty with "Te" -> get_part lr side te | _ -> te),
        (if side = "bar" then ""
         else match ty with "It" -> get_part lr side it | _ -> it),
        if side = "bar" then ""
        else match ty with "Im" -> get_part lr side im | _ -> im ))
    row

let split_rows_with_vbar tree =
  List.fold_left
    (fun acc row ->
      let lr, bar = scan_row_for_bar row in
      if bar then
        if lr = 0 then copy_row row lr "bar" :: copy_row row lr "right" :: acc
        else copy_row row lr "left" :: copy_row row lr "bar" :: acc
      else row :: acc)
    [] (List.rev tree)

let get_nb_full_col_in_span cols b s =
  let rec loop i n = function
    | [] -> n
    | col :: cols ->
        if i < b then loop (i + 1) n cols
        else if i < b + s then
          loop (i + 1) (if col.[0] = 'F' then n + 1 else n) cols
        else n
  in
  loop 0 0 cols

let split_tree (conf : Config.config) tree =
  let row1 = List.nth tree 0 in
  let row2 = List.nth tree 1 in
  let row3 = List.nth tree 2 in
  let desc_tree_1, desc_tree_2, desc_tree_3 =
    match (List.length row1, List.length row2, List.length row3) with
    | 1, 1, 1 -> (false, false, true)
    | 1, 1, _ -> (false, true, false)
    | 1, _, _ -> (true, false, false)
    | _ -> (false, false, false)
  in
  nb_head_rows :=
    if desc_tree_1 then 1
    else if desc_tree_2 then 2
    else if desc_tree_3 then 3
    else 0;
  let new_tree =
    if desc_tree_2 || desc_tree_3 then
      if List.length tree < 4 then tree
      else
        let rec copy_tree i new_tree = function
          | [] -> List.rev new_tree
          | row :: tree ->
              if i < 3 then copy_tree (i + 1) new_tree tree
              else copy_tree (i + 1) (row :: new_tree) tree
        in
        copy_tree 0 [] tree
    else tree
  in
  let cols = find_empty_columns new_tree 0 in
  let nb_cols = List.length cols in
  let col_middle = (nb_cols / 2) - conf.split in
  let row_split _c i1 i2 row =
    let rec loop i new_row = function
      | [] -> List.rev new_row
      | (w, s, ty, te, it, im) :: row when i1 = 0 && i + s <= i2 ->
          if s <= 0 then
            Printf.eprintf "Neg span left 1: i2:%d, i:%d, s:%d\n" i2 i s;
          loop (i + s) ((w, s, ty, te, it, im) :: new_row) row
      | (_w, s, _ty, _te, _it, _im) :: row when i1 = 0 && i >= i2 ->
          loop (i + s) new_row row
      | (w, s, ty, te, it, im) :: row when i1 = 0 && i + s > i2 ->
          if i2 - i <= 0 then
            Printf.eprintf "Neg span left 2: i2:%d, i:%d, s:%d\n" i2 i s;
          loop (i + s) ((w, i2 - i, ty, te, it, im) :: new_row) row
      | (w, s, ty, te, it, im) :: row when i1 <> 0 && i >= i1 ->
          if s <= 0 then
            Printf.eprintf "Neg span right 1: i:%d, s:%d, i1:%d\n" i s i1;
          loop (i + s) ((w, s, ty, te, it, im) :: new_row) row
      | (_w, s, _ty, _te, _it, _im) :: row when i1 <> 0 && i + s <= i1 ->
          loop (i + s) new_row row
      | (w, s, ty, te, it, im) :: row when i1 <> 0 && i + s >= i1 ->
          if i + s - i1 <= 0 then
            Printf.eprintf "Neg span right 2: i:%d, s:%d, i1:%d\n" i s i1;
          loop (i + s) ((w, i + s - i1, ty, te, it, im) :: new_row) row
      | (_w, s, _ty, _te, _it, _im) :: _row ->
          Printf.eprintf "Assert: i=%d, i1=%d, i2=%d, s=%d\n" i i1 i2 s;
          assert false
    in
    loop 0 [] row
  in
  let rec tree_split i i1 i2 new_tree = function
    | [] -> List.rev new_tree
    | row :: tree ->
        tree_split (i + 1) i1 i2 (row_split i i1 i2 row :: new_tree) tree
  in
  (* Left half:  columns [0, col_middle]   — inclusive upper bound
     Right half: columns [col_middle+1, nb_cols-1]
     Using col_middle+1 as i2 for the left ensures the cell AT col_middle
     is fully included rather than truncated to span 0 and dropped.    *)
  let tree_left = tree_split 0 0 (col_middle + 1) [] new_tree in
  let tree_right = tree_split 0 (col_middle + 1) (nb_cols - 1) [] new_tree in
  let row_left = function
    | [ (w, _s, ty, te, it, im) ] ->
        (* left half spans col_middle+1 cols; subtract 2 for padding E cells *)
        [ (1, 2, "E", "", "", ""); (w, col_middle - 1, ty, te, it, im) ]
    | _ -> assert false
  in
  let row_right = function
    | [ (w, _s, ty, te, it, im) ] ->
        [
          (w, nb_cols - col_middle - 3, ty, te, it, im); (1, 2, "E", "", "", "");
        ]
    | _ -> assert false
  in
  if desc_tree_2 then
    ( [ row_left row1; row_left row2 ] @ tree_left,
      [ row_right row1; row_right row2 ] @ tree_right )
  else if desc_tree_3 then
    ( [ row_left row1; row_left row2; row_left row3 ] @ tree_left,
      [ row_right row1; row_right row2; row_right row3 ] @ tree_right )
  else (tree_left, tree_right)

let get_img_name base_name im =
  let ext = ".jpg" in
  let where = "images" in
  let href_attrl = Hutil.split_href im in
  let k = Hutil.get_href_attr "k" href_attrl in
  let name =
    Format.sprintf "%s"
      (String.concat Filename.dir_sep [ "."; where; base_name; k ^ ext ])
  in
  Sutil.replace_str "\\_{}" "_" name

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

let expand_end_cells row =
  let absorb_right = function
    | (w1, s1, ty1, te1, it1, im1) :: (_, s2, ty2, _, _, _) :: row
      when ty2 = "E" ->
        (w1, s1 + s2, ty1, te1, it1, im1) :: row
    | row -> row
  in
  row |> absorb_right |> List.rev |> absorb_right |> List.rev

let expand_cells _conf tree =
  let rec expand row new_row =
    match row with
    | (w1, s1, ty1, te1, it1, im1)
      :: (w2, s2, ty2, te2, it2, im2)
      :: (w3, s3, ty3, te3, it3, im3)
      :: row -> (
        match (ty1, ty2, ty3) with
        | ty1, ty2, ty3 when ty1 = "E" && ty3 = "E" && s1 = 1 && s3 = 1 ->
            expand row ([ (w2, s2 + 2, ty2, te2, it2, im2) ] @ new_row)
        | ty1, ty2, ty3 when ty1 = "E" && ty3 = "E" && s3 = 1 ->
            expand row
              ([
                 (w1, s1 - 1, ty1, te1, it1, im1);
                 (w2, s2 + 2, ty2, te2, it2, im2);
               ]
              @ new_row)
        | ty1, ty2, ty3 when ty1 = "E" && ty3 = "E" && s1 = 1 ->
            expand row
              ([
                 (w2, s2 + 2, ty2, te2, it2, im2);
                 (w3, s3 - 1, ty3, te3, it3, im3);
               ]
              @ new_row)
        | ty1, ty2, ty3 when ty1 = "E" && ty3 = "E" ->
            expand row
              ([
                 (w1, s1 - 1, ty1, te1, it1, im1);
                 (w2, s2 + 2, ty2, te2, it2, im2);
                 (w3, s3 - 1, ty3, te3, it3, im3);
               ]
              @ new_row)
        | ty1, ty2, _ty3 when ty1 = "E" && ty3 = "E" ->
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
      | row :: tree -> loop tree (expand_end_cells (expand row []) :: new_tree)
    in
    loop tree []
  in
  List.rev tree

let merge_cells _conf tree =
  let rec merge row new_row =
    match row with
    | [] -> List.rev new_row
    | (w1, s1, ty1, te1, it1, im1) :: (_w2, s2, ty2, te2, it2, im2) :: row
      when ty1 = ty2 && te1 = "" && te2 = "" && it1 = "" && it2 = "" && im1 = ""
           && im2 = "" ->
        merge ((w1, s1 + s2, ty1, te1, it1, im1) :: row) new_row
    | (w1, s1, ty1, te1, it1, im1)
      :: (_w2, s2, ty2, _te2, _it2, _im2)
      :: (_w3, s3, ty3, te3, it3, im3)
      :: row
      when ty1 = ty3 && ty2 = "E" && te1 = "" && te3 = "" && it1 = ""
           && it3 = "" && im1 = "" && im3 = "" ->
        merge ((w1, s1 + s2 + s3, ty1, te1, it1, im1) :: row) new_row
    | (w1, s1, ty1, te1, it1, im1)
      :: (_w2, s2, _ty2, _te2, _it2, _im2)
      :: (w3, s3, ty3, te3, it3, im3)
      :: row
      when ty1 = "Hl" && _ty2 = "E" && ty3 = "Hc" ->
        merge row
          ((w3, s2 + s3, ty3, te3, it3, im3)
          :: (w1, s1, ty1, te1, it1, im1)
          :: new_row)
    | (w1, s1, ty1, te1, it1, im1)
      :: (_w2, s2, _ty2, _te2, _it2, _im2)
      :: (w3, s3, ty3, te3, it3, im3)
      :: row
      when ty1 = "Hc" && _ty2 = "E" && ty3 = "Hr" ->
        merge row
          ((w3, s3, ty3, te3, it3, im3)
          :: (w1, s1 + s2, ty1, te1, it1, im1)
          :: new_row)
    | (w1, s1, ty1, te1, it1, im1) :: (w2, s2, ty2, te2, it2, im2) :: row ->
        merge
          ((w2, s2, ty2, te2, it2, im2) :: row)
          ((w1, s1, ty1, te1, it1, im1) :: new_row)
    | (w1, s1, ty1, te1, it1, im1) :: row ->
        merge row ((w1, s1, ty1, te1, it1, im1) :: new_row)
  in
  let rec loop tree new_tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree -> loop tree (merge row [] :: new_tree)
  in
  loop tree []

let double_each_cell tree =
  let rec scan_tree tree new_tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree ->
        let new_row =
          let rec scan_row row new_row =
            match row with
            | [] -> List.rev new_row
            | (a, s, typ, c, d, e) :: row when typ = "Hr" ->
                scan_row row
                  ((a, s, "Hr", c, d, e) :: (a, s, "E", c, d, e) :: new_row)
            | (a, s, typ, c, d, e) :: row when typ = "Hl" ->
                scan_row row
                  ((a, s, "E", c, d, e) :: (a, s, "Hl", c, d, e) :: new_row)
            | (a, s, typ, c, d, e) :: row ->
                scan_row row ((a, s * 2, typ, c, d, e) :: new_row)
          in
          scan_row row []
        in
        scan_tree tree (new_row :: new_tree)
  in
  scan_tree tree []

let flip_tree_h tree =
  let flip row = List.fold_left (fun acc cell -> cell :: acc) [] row in
  let rec loop tree new_tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree -> loop tree (flip row :: new_tree)
  in
  loop tree []

let split_hr_cells tree =
  let rec split row new_row =
    match row with
    | [] -> List.rev new_row
    | (a, s, typ, c, d, e) :: row when typ = "Hr" ->
        let half_s = s / 2 in
        if s / 2 = half_s then
          split row
            ((a, half_s, "Hc", c, d, e)
            :: (a, s - half_s, "E", c, d, e)
            :: new_row)
        else
          split row
            ((a, half_s, "Hc", c, d, e) :: (a, 1, "Hr", c, d, e)
            :: (a, s - half_s, "E", c, d, e)
            :: new_row)
    | (a, s, typ, c, d, e) :: row when typ = "Hl" ->
        let half_s = s / 2 in
        if s / 2 = half_s then
          split row
            ((a, s - half_s, "E", c, d, e)
            :: (a, half_s, "Hc", c, d, e) :: new_row)
        else
          split row
            ((a, half_s, "E", c, d, e) :: (a, 1, "Hl", c, d, e)
            :: (a, s - half_s, "Hc", c, d, e)
            :: new_row)
    | cell :: row -> split row (cell :: new_row)
  in
  let rec loop tree new_tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree -> loop tree (split row [] :: new_tree)
  in
  loop tree []

let remove_duplicate_rows tree =
  let rec loop prev = function
    | [] -> List.rev prev
    | row :: tree ->
        (* Keep bar rows even if identical — they may connect different layers *)
        let keep =
          match prev with
          | [] -> true
          | last :: _ -> row <> last || classify_row row = Bar
        in
        if keep then loop (row :: prev) tree else loop prev tree
  in
  loop [] tree

let get_nb_head_rows tree =
  if List.length tree > 3 then
    let row1 = List.nth tree 0 in
    let row2 = List.nth tree 1 in
    let row3 = List.nth tree 2 in
    match (List.length row1, List.length row2, List.length row3) with
    | 1, 1, 1 -> 3
    | 1, 1, _ -> 2
    | 1, _, _ -> 1
    | _ -> 0
  else 0

let test_zero_span_r row =
  let rec loop ok = function
    | [] -> ok
    | (_w, s, _ty, _te, _it, _im) :: row ->
        if s = 0 then (
          Printf.eprintf "Zero span!!\n";
          loop false row)
        else if s < 0 then (
          Printf.eprintf "Negative span!!\n";
          loop false row)
        else loop ok row
  in
  loop true row

let test_zero_span_t tree comment =
  let ok =
    let rec loop ok = function
      | [] -> ok
      | row :: tree -> loop (ok && test_zero_span_r row) tree
    in
    loop true tree
  in
  if not ok then Printf.eprintf "Zero or negative span at %s\n" comment

let print_tab_env cols tabular_env =
  let tab_env =
    let rec loop i j0 j1 acc1 =
      let j = try String.index_from tabular_env j1 'P' with Not_found -> -1 in
      if j = -1 then
        acc1 ^ "\\par\n"
        ^ String.sub tabular_env
            (if j0 = 0 then j0 else j0 - 1)
            (String.length tabular_env - j0)
      else if i = 12 then
        loop 0 (j + 1) (j + 1)
          (acc1 ^ "\\par\n"
          ^ String.sub tabular_env (if j0 = 0 then j0 else j0 - 1) (j - j0))
      else loop (i + 1) j0 (j + 1) acc1
    in
    loop 0 0 0 ""
  in
  (String.concat ", " cols, tab_env)

let squeeze_row row_init =
  let rec loop state new_row = function
    | [] -> if state = 2 then List.rev new_row else row_init
    | (a, s, typ, c, d, e) :: row -> (
        match typ with
        | ("E" | "Vr1") when state = 0 ->
            loop 1 ((a, s, typ, c, d, e) :: new_row) row
        | ("Hr" | "Hl" | "Hc") when state = 0 ->
            loop 2 ((a, s, typ, c, d, e) :: new_row) row
        | ("Te" | "It" | "Im") when state = 0 ->
            loop 0 ((a, s, typ, c, d, e) :: new_row) row
        | ("E" | "Vr1") when state = 1 ->
            loop 1 ((a, s, typ, c, d, e) :: new_row) row
        | ("Te" | "It" | "Im") when state = 1 ->
            loop 0 ((a, s, typ, c, d, e) :: new_row) row
        | ("Hr" | "Hl" | "Hc") when state = 1 ->
            loop 2 ((a, s, typ, c, d, e) :: new_row) row
        | "Vr1" when state = 2 -> loop 2 ((a, s, "Vr2", c, d, e) :: new_row) row
        | "E" when state = 2 -> loop 2 ((a, s, typ, c, d, e) :: new_row) row
        | ("Hr" | "Hl" | "Hc") when state = 2 ->
            loop 2 ((a, s, typ, c, d, e) :: new_row) row
        | ("Te" | "It" | "Im") when state = 2 ->
            loop 0 ((a, s, typ, c, d, e) :: new_row) row
        | _ -> loop state ((a, s, typ, c, d, e) :: new_row) row)
  in
  loop 0 [] row_init

let squeeze_row_tree tree =
  let rec loop new_tree = function
    | [] -> List.rev new_tree
    | row :: tree -> loop (squeeze_row row :: new_tree) tree
  in
  loop [] tree
