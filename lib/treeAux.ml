(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)

let row_nb = ref 0
let nb_head_rows = ref 0

(* TODO Imagek = Portrait ?? no *)
type im_type = Portrait | Imagek | Images | Vignette

type image = {
  im_type : im_type;
  filename : string;
  where : int * int * int * int; (* ch, sec, ssec, sssec*)
  image_nbr : int;
}

(* width, span, (E O Hr Hc Hl Vc Im), item, text, image *)
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

let row_width row = List.fold_left (fun w (_, s, _, _, _, _) -> w + s) 0 row

let nbr_empty_cols cols =
  List.fold_left (fun n col -> if col.[0] = 'E' then n + 1 else n) 0 cols

let print_row row n =
  Printf.eprintf "Rox %d = \n" n;
  List.iter
    (fun (_, s, ty, te, it, im) ->
      let te = Sutil.clean_double_back_slash_2 te |> Sutil.clean_item in
      let it = Sutil.clean_double_back_slash_2 it |> Sutil.clean_item in
      Printf.eprintf "[ (%d) %s te:%s it:%s im:%s], " s ty te it im)
    row;
  Printf.eprintf "\n"

let row_span _cols row _n =
  let rec loop span row =
    match row with
    | [] -> span
    | (_, s, _, _, _, _) :: row -> loop (span + s) row
  in
  loop 0 row

let print_row_span cols row n =
  Printf.eprintf "Row %d: %d cells, span %d\n" n (List.length row)
    (row_span cols row n)

let print_tree_rows_span cols tree =
  let rec loop i tree =
    match tree with
    | [] -> ()
    | row :: tree ->
        print_row_span cols row i;
        loop (i + 1) tree
  in
  loop 0 tree

let print_tree tree n =
  Printf.eprintf "print tree (%d)\n" n;
  let rec loop i tree =
    match tree with
    | [] -> ()
    | row :: tree when i < n ->
        print_row row i;
        loop (i + 1) tree
    | _ -> ()
  in
  loop 0 tree

let test_tree_width tree nb_head_rows =
  let rec loop first i w0 tree =
    match tree with
    | [] -> (i, w0, w0, true)
    | row :: tree when i >= nb_head_rows ->
        let w = row_width row in
        if first then loop false (i + 1) w tree
        else if w <> w0 then (i + 1, w, w0, false) (* unballanced tree *)
        else loop false (i + 1) w0 tree
    | _row :: tree -> loop true (i + 1) w0 tree
  in
  loop true 0 0 tree

(* no test on i relative to nb cols! *)
let is_empty_cell row i =
  let rec loop j row =
    match row with
    | [] -> false
    | (_, s, ty, _, _, _) :: row ->
        (* loop to column i span by span *)
        if j + s < i then loop (j + s) row
        else
          (* inch one by one to i within span *)
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
  (*Printf.eprintf "update cols, row:%d; row len:%d, cols len: %d, span:%d\n"
    row_nb (List.length row) (List.length old_cols)
    (row_span old_cols row row_nb);*)
  let rec loop cols i row =
    match row with
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

(* an empty column is a column with only E, Hc, Hr -- *)
(* scan the whole tree row by row to determine empty columns *)
let find_empty_columns tree nb_head_rows =
  (*Printf.eprintf "find empty cols, tree depth %d\n" (List.length tree);*)
  (* i is row number *)
  let _i, width, _w0, ok = test_tree_width tree nb_head_rows in
  let cols =
    if ok then
      (* set all columns to empty *)
      let rec loop n acc = if n = 0 then acc else loop (n - 1) ("E" :: acc) in
      loop width []
    else (
      Printf.eprintf "Unbalanced tree (find empty cols)\n";
      [])
  in
  let rec loop i cols tree =
    match tree with
    | [] -> cols
    | row :: tree when i >= nb_head_rows ->
        let cols = update_cols cols row i in
        loop (i + 1) cols tree
    | _row :: tree -> loop (i + 1) cols tree
  in
  loop 0 cols tree

let remove_empty_cells cols row _n =
  let new_row =
    let rec loop i new_row row =
      match row with
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
            loop (i + s) ((a, s - p, typ, c, d, e) :: new_row) row
    in
    loop 0 [] row
  in
  new_row

(* FIXME adjust span of head rows accorrdingly *)
let remove_empty_cols _conf tree nb_head_rows =
  let cols = find_empty_columns tree nb_head_rows in
  let tree2 =
    let rec loop i new_tree tree =
      match tree with
      | [] -> List.rev new_tree
      | row :: tree when i >= nb_head_rows ->
          loop (i + 1) (remove_empty_cells cols row i :: new_tree) tree
      | row :: tree -> loop (i + 1) (row :: new_tree) tree
    in
    loop 0 [] tree
  in
  (* now fix head rows *)
  let cols = find_empty_columns tree2 nb_head_rows in
  let new_span = List.length cols in
  let new_tree =
    let rec loop i new_tree tree =
      match tree with
      | [] -> List.rev new_tree
      | [ (w, _s, ty, te, it, im) ] :: tree when i < nb_head_rows ->
          loop (i + 1) ([ (w, new_span, ty, te, it, im) ] :: new_tree) tree
      | row :: tree -> loop (i + 1) (row :: new_tree) tree
    in
    loop 0 [] tree2
  in
  new_tree

let get_part lr side str =
  let str = Sutil.clean_double_back_slash str in
  let str = Sutil.suppress_trailing_sp str in
  let str = Sutil.suppress_leading_sp str in
  let str = Sutil.replace_utf8_bar str in
  let res =
    let len = String.length str in
    match (lr, side) with
    | 0, "left" when len > 1 -> String.sub str 1 (String.length str - 1)
    | 0, "right" when len > 1 -> String.sub str 1 (String.length str - 1)
    | 1, "left" when len > 1 -> String.sub str 0 (String.length str - 1)
    | 1, "right" when len > 1 -> String.sub str 0 (String.length str - 1)
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
        (if side = "bar" then match ty with "Te" | "It" -> "Vr1" | _ -> ty
        else ty),
        (if side = "bar" then ""
        else match ty with "Te" -> get_part lr side te | _ -> te),
        (if side = "bar" then ""
        else match ty with "It" -> get_part lr side it | _ -> it),
        if side = "bar" then ""
        else match ty with "Im" -> get_part lr side im | _ -> im ))
    row

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
    | col :: cols ->
        if i < b then loop (i + 1) n cols
        else if i < b + s then
          if col.[0] = 'F' then loop (i + 1) (n + 1) cols
          else loop (i + 1) n cols
        else n
  in
  loop 0 0 cols

(* split trees (Desc) whose first (3) rows are a single cell *)
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
  (* define new_tree (without first three lines *)
  let new_tree =
    if desc_tree_2 || desc_tree_3 then
      if List.length tree < 4 then tree
      else
        let rec copy_tree i new_tree tree =
          match tree with
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
  (* TODO see if better column *)
  let row_split _c i1 i2 row =
    let rec loop i new_row row =
      match row with
      | [] -> List.rev new_row (* done *)
      | (w, s, ty, te, it, im) :: row when i1 = 0 && i + s <= i2 ->
          (* left part *)
          if s <= 0 then
            Printf.eprintf "Neg span left 1: i2:%d, i:%d, s:%d\n" i2 i s;
          loop (i + s) ((w, s, ty, te, it, im) :: new_row) row
      | (_w, s, _ty, _te, _it, _im) :: row when i1 = 0 && i >= i2 ->
          (* ignore *)
          loop (i + s) new_row row
      | (w, s, ty, te, it, im) :: row when i1 = 0 && i + s > i2 ->
          (* split *)
          if i2 - i <= 0 then
            Printf.eprintf "Neg span left 2: i2:%d, i:%d, s:%d\n" i2 i s;
          loop (i + s) ((w, i2 - i, ty, te, it, im) :: new_row) row
      | (w, s, ty, te, it, im) :: row when i1 <> 0 && i >= i1 ->
          (* right part *)
          if s <= 0 then
            Printf.eprintf "Neg span right 1: i:%d, s:%d, i1:%d\n" i s i1;
          loop (i + s) ((w, s, ty, te, it, im) :: new_row) row
      | (_w, s, _ty, _te, _it, _im) :: row when i1 <> 0 && i + s <= i1 ->
          (* ignore *)
          loop (i + s) new_row row
      | (w, s, ty, te, it, im) :: row when i1 <> 0 && i + s >= i1 ->
          (* split *)
          if i + s - i1 <= 0 then
            Printf.eprintf "Neg span right 2: i:%d, s:%d, i1:%d\n" i s i1;
          loop (i + s) ((w, i + s - i1, ty, te, it, im) :: new_row) row
      | (_w, s, _ty, _te, _it, _im) :: _row ->
          Printf.eprintf "Assert: i=%d, i1=%d, i2=%d, s=%d\n" i i1 i2 s;
          assert false
    in
    loop 0 [] row
  in

  let rec tree_split i i1 i2 new_tree tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree ->
        tree_split (i + 1) i1 i2 (row_split i i1 i2 row :: new_tree) tree
  in

  let tree_left = tree_split 0 0 col_middle [] new_tree in
  let tree_right = tree_split 0 (col_middle + 1) (nb_cols - 1) [] new_tree in

  (* redefine new first rows *)
  let row_left row =
    match row with
    | [ (w, _s, ty, te, it, im) ] ->
        [ (1, 2, "E", "", "", ""); (w, col_middle - 2, ty, te, it, im) ]
    | _ -> assert false
  in
  let row_right row =
    match row with
    | [ (w, _s, ty, te, it, im) ] ->
        [
          (w, nb_cols - col_middle - 3, ty, te, it, im); (1, 2, "E", "", "", "");
        ]
    | _ -> assert false
  in
  if desc_tree_2 then
    let row1_left = row_left row1 in
    let row2_left = row_left row2 in
    let row1_right = row_right row1 in
    let row2_right = row_right row2 in
    let tree_left = [ row1_left; row2_left ] @ tree_left in
    let tree_right = [ row1_right; row2_right ] @ tree_right in
    (tree_left, tree_right)
  else if desc_tree_3 then
    let row1_left = row_left row1 in
    let row2_left = row_left row2 in
    let row3_left = row_left row3 in
    let row1_right = row_right row1 in
    let row2_right = row_right row2 in
    let row3_right = row_right row3 in
    let tree_left = [ row1_left; row2_left; row3_left ] @ tree_left in
    let tree_right = [ row1_right; row2_right; row3_right ] @ tree_right in
    (tree_left, tree_right)
  else (tree_left, tree_right)

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
  Sutil.replace_str "\\_{}" "_" name

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

let expand_end_cells row =
  let row =
    match row with
    | (w1, s1, ty1, te1, it1, im1) :: (_, s2, ty2, _, _, _) :: row
      when ty2 = "E" ->
        (w1, s1 + s2, ty1, te1, it1, im1) :: row
    | _ -> row
  in
  let row = List.rev row in
  let row =
    match row with
    | (w1, s1, ty1, te1, it1, im1) :: (_, s2, ty2, _, _, _) :: row
      when ty2 = "E" ->
        (w1, s1 + s2, ty1, te1, it1, im1) :: row
    | _ -> row
  in
  List.rev row

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
                 (w3, s3 - 1, ty3, te3, it3, im3);
                 (w2, s2 + 2, ty2, te2, it2, im2);
               ]
              @ new_row)
        | ty1, ty2, ty3 when ty1 = "E" && ty3 = "E" ->
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
      | row :: tree ->
          let new_row = expand_end_cells (expand row []) in
          loop tree (new_row :: new_tree)
    in
    loop tree []
  in
  List.rev tree

let merge_cells _conf tree =
  (* TODO recompute half cell rules *)
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
      :: (_w2, s2, ty2, _te2, _it2, _im2)
      :: (w3, s3, ty3, te3, it3, im3)
      :: row
      when ty1 = "Hl" && ty2 = "E" && ty3 = "Hc" ->
        merge row
          ((w3, s2 + s3, ty3, te3, it3, im3)
          :: (w1, s1, ty1, te1, it1, im1)
          :: new_row)
    | (w1, s1, ty1, te1, it1, im1)
      :: (_w2, s2, ty2, _te2, _it2, _im2)
      :: (w3, s3, ty3, te3, it3, im3)
      :: row
      when ty1 = "Hc" && ty2 = "E" && ty3 = "Hr" ->
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
  let rec flip row new_row =
    match row with [] -> new_row | cell :: row -> flip row (cell :: new_row)
  in
  let rec loop tree new_tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree -> loop tree (flip row [] :: new_tree)
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
  let rec loop prev tree new_tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree ->
        if row = prev then loop row tree new_tree
        else loop row tree (row :: new_tree)
  in
  loop [] tree []

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
  let rec loop ok row =
    match row with
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
    let rec loop ok tree =
      match tree with
      | [] -> ok
      | row :: tree -> loop (ok && test_zero_span_r row) tree
    in
    loop true tree
  in
  if ok then () else Printf.eprintf "Zero or negative span at %s\n" comment

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
  let rec loop state new_row row =
    match row with
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
  let rec loop new_tree tree =
    match tree with
    | [] -> List.rev new_tree
    | row :: tree -> loop (squeeze_row row :: new_tree) tree
  in
  loop [] tree
