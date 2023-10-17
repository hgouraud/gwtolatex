(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)

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

let print_tree tree =
  let i, w, w0, ok = test_tree_width tree in
  if not ok then (
    Printf.eprintf "Unbalanced tree, row %d w=%d, w0=%d\n" i w w0;
    exit 1);
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
