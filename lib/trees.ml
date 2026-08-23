(* tree construction tools *)
(* v1  Henri, 2023/10/16 *)
(* v2  Henri+Claude, 2026/05 — dynamic Vr heights via row classification *)
open TreeAux

let row_width row = List.fold_left (fun w (_, s, _, _, _, _) -> w + s) 0 row
let row_nb = ref 0
let nb_head_rows = ref 0

(* Start the first tree half on a fresh page. Forcing this wastes the space
   under a preceding section title, so it defaults OFF; instead shrink the
   first half with clip_fold (ClipOffset) so it fits below the title. Toggle
   with <x TreeNewPage on|off>. *)
let tree_newpage = ref false

(* Fold shift (conf.unit, cm): move the clip fold - and the star marker - off
   the tree centre, growing the left half and shrinking the right by this
   amount (negative does the reverse). Use it to rebalance the two halves,
   e.g. to make the first (title-sharing) half smaller so it fits without a
   forced page break. Set via <x ClipOffset x>. *)
let clip_fold = ref 0.0

(* ── Two-page by clipping (instead of splitting) ─────────────────
   The whole tree is laid out ONCE, ~two pages wide, into a save-box;
   each page then shows a clipped, translated window of it. Every person
   is drawn a single time at its true position, so straddling people no
   longer have a placement problem. clip_overlap (cm) makes each window
   reach a little past the fold, so a name/portrait on the cut is
   readable on both pages instead of sliced. Requires \usepackage{adjustbox}
   in the preamble. Toggle/param via <x ClipMode on|off> and
   <x ClipOverlap 1.0>. *)
let clip_mode = ref true
let clip_overlap = ref 1.0

(* Extra width (conf.unit, cm) of page margin each clipped window may use. The
   tree is laid out 2 x this wider (one margin each side), so every column -
   notably the crowded top row - gets more room, and the width cap is raised by
   the same amount so the wider window spills into the margin instead of being
   scaled back to \textwidth. Set via <x ClipEnlarge 1.5>. *)
let clip_enlarge = ref 0.0

(* Per-page horizontal nudge (conf.unit, cm) for the clipped windows, applied
   as \hspace* before each. Left the left window, right the right window. Set
   via <x ClipShiftLeft x> / <x ClipShiftRight x>. *)
let clip_shift_l = ref 0.0
let clip_shift_r = ref 0.0

(* Padding (conf.unit, cm) added to each side of the saved tree box before
   clipping, so an outermost name that spills past its column is not shaved
   off by the clip. Independent left/right: the left edge rarely overflows, so
   it defaults to 0; the right often needs a margin. The fold math below
   corrects for any left/right asymmetry so the seam stays on the tree's true
   centre. Set via <x ClipPadLeft x> / <x ClipPadRight x> (or <x ClipPad x>
   for both). *)
let clip_pad_l = ref 0.0
let clip_pad_r = ref 1.5

(* Length (conf.unit, cm) of the centre fold ticks: short marks overlaid at
   the top and bottom of the tree's horizontal centre - i.e. the clip fold.
   Because the centre falls inside the overlap on both pages, both pages show
   them at the same tree position, giving the reader a registration cue to
   align the two sheets. 0 = no ticks. Set via <x ClipTick x>. *)
let clip_tick = ref 0.35

(* Where to draw the fold star: "top", "bottom", "both", "off", or "auto".
   The root person is a single wide cell at one end of the tree (bottom for an
   ascendant tree, top for a descendant one) and a star there lands on its
   name; "auto" puts the star at the OTHER end (the many-people end) - top for
   ascendants, bottom for descendants - and falls back to "both" when the two
   ends are equally busy. Set via <x MidPoint top|bottom|both|off|auto>. *)
let clip_midpoint = ref "auto"

(* Draw \fbox frames around layout elements - every cell (so names/images
   show their box), and each clipped window - to help tune the offset, pad,
   overlap and scale parameters visually. Set via <x ShowBoxes on|off>. *)
let show_boxes = ref false

(* Internal: when >0, init_cols uses this as the tree width (the clip path
   renders the full tree ~two pages wide). when true, mode_1 omits its own
   \begin{sideways} wrapper because the clip path rotates each window. *)
let clip_render_width = ref 0.0
let no_sideways_wrap = ref false

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
      if !clip_render_width > 0.0 then !clip_render_width
      else if conf.sideways then conf.textheight
      else conf.textwidth
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

    (* Index of the bottom-most Content row: the root person (Emile Martin),
       a single full-width cell duplicated on both split halves. Used to nudge
       it toward the seam so it reads as the shared join, not as belonging to
       one page. *)
    let last_content_ri =
      let rec find i =
        if i < 0 then -1
        else if classify_row rows.(i) = Content then i
        else find (i - 1)
      in
      find (Array.length rows - 1)
    in

    let offset_b =
      if conf.hoffset <> 0. then
        Format.sprintf "\\hspace*{%1.2f%s}\n" conf.hoffset conf.unit
      else ""
    in
    let tabular_b =
      Format.sprintf
        "%s\\nohyphens\\newcolumntype{P}[1]{>{\\centering\\arraybackslash}p{#1}}\n\
         \\renewcommand*{\\arraystretch}{0.1}\\renewcommand*{\\tabcolsep}{%1.2f%s}%s\\begin{tabular}{%s}\n"
        (if conf.sideways && not !no_sideways_wrap then "\\begin{sideways}"
         else "")
        conf.colsep conf.unit offset_b tabular_env
    in
    let tabular_e =
      Format.sprintf "\\end{tabular}%s\n\\hyphenation{nor-mal-ly}\n"
        (if conf.sideways && not !no_sideways_wrap then "\\end{sideways}\n"
         else "")
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
                  let fbox_b =
                    if conf.debug = 999 || !show_boxes then "\\fbox{" else ""
                  in
                  let fbox_e =
                    if conf.debug = 999 || !show_boxes then "}" else ""
                  in
                  (* The font-size switch and \linespread are opened HERE,
                     inside the minipage group, so both are still active at
                     \par (\end{center}); a size switch that closes earlier
                     (the old inline font_b/font_e) let the leading revert
                     to the outer size, so small text got large leading.
                     linespread tightens the interline; 0.80 is the knob. *)
                  let minipage_b =
                    Format.sprintf
                      "%s\\begin{minipage}{%1.2f%s}\\begin{center}%s\\linespread{0.80}\\selectfont "
                      fbox_b
                      (colwidth *. Float.of_int s)
                      conf.unit
                      (if conf.fontsize = "" then "" else "\\" ^ conf.fontsize)
                  in
                  let minipage_e =
                    Format.sprintf "\\end{center}\\end{minipage}%s" fbox_e
                  in
                  (* {\tiny ...} not \tiny{...}: size commands are
                     switches, the braces must enclose the switch or it
                     leaks past its intended scope *)
                  let font_b =
                    if conf.fontsize = "" then ""
                    else "{\\" ^ conf.fontsize ^ " "
                  in
                  let font_e = if conf.fontsize = "" then "" else "}" in

                  (* ── Horizontal rule helper ──
                     End segments must be FLUSH with the column edge they
                     share with the neighbouring full rule. The old
                     colwidth/4 hspace left the half-rule floating
                     centered-ish, colwidth/8 short of the edge on both
                     sides: visibly detached "stray" dashes at the two
                     ends of every branch. hspace* (not hspace): the glue
                     sits at a line edge and plain \hspace would be
                     dropped there. *)
                  let hr s lrc =
                    (* Every rule overhangs its cell by colsep on the
                       side(s) where it meets a neighbour: adjacent cells
                       are separated by 2 x \tabcolsep, so without the
                       overhang the branch line shows a 2*colsep nick at
                       every column boundary. Negative \hspace* keeps the
                       line's natural width equal to the column width, so
                       centering is unaffected. *)
                    let cs = conf.colsep in
                    let half = colwidth /. 2.0 in
                    let u = conf.unit in
                    (* Segments are built inside \makebox (an \hbox):
                       in paragraph mode TeX DISCARDS a trailing glue at
                       \par, so an end-of-cell \hspace* (the Hl right
                       spacer) silently vanished and the half-rule got
                       re-centered, overshooting its connector. Inside a
                       box, glue is never discarded, and the box's fixed
                       width (= column width) keeps centering exact. *)
                    let mbox body =
                      Format.sprintf "\\makebox[%1.2f%s][l]{%s}" colwidth u body
                    in
                    let rule_w w =
                      Format.sprintf "\\rule[0pt]{%1.2f%s}{%1.2fpt}" w u
                        conf.rulethickns
                    in
                    let hsp w = Format.sprintf "\\hspace*{%1.2f%s}" w u in
                    let seg =
                      match lrc with
                      | "e" -> ""
                      | "c" ->
                          (* full-width rule, bridging both column gaps *)
                          mbox
                            (hsp (-.cs)
                            ^ rule_w (colwidth +. (2.0 *. cs))
                            ^ hsp (-.cs))
                      | "r" ->
                          (* rule on the right half, flush with the right
                             edge and bridging toward the next cell *)
                          mbox (hsp half ^ rule_w (half +. cs) ^ hsp (-.cs))
                      | "l" ->
                          (* rule on the left half, flush with the left
                             edge and bridging toward the previous cell *)
                          mbox (hsp (-.cs) ^ rule_w (half +. cs) ^ hsp half)
                      | _ -> ""
                    in
                    let rec loop i acc =
                      if i = s then acc
                      else
                        loop (i + 1) (acc ^ seg ^ if i + 1 = s then "" else "&")
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
                          let str =
                            match (te, it) with
                            | "", it when it <> "" -> font_b ^ it ^ font_e
                            | te, "" when te <> "" -> font_b ^ te ^ font_e
                            | te, it when te <> "" && it <> "" ->
                                font_b ^ te ^ "\\\\" ^ it ^ font_e
                            | "", "" -> ""
                            | _, _ -> font_b ^ te ^ it ^ font_e
                          in
                          (* A portrait is carried in the im field (set by
                               the dag translator) and prepended on its own
                               line. The break is added HERE, after te/it have
                               been through clean_double_back_slash_2, so it
                               survives - a \\ placed inside te would be
                               stripped by that cleaning. *)
                          let str =
                            if im = "" then str
                            else if str = "" then im
                            else im ^ "\\\\" ^ str
                          in
                          if
                            conf.twopages && (not !clip_mode)
                            && ri = last_content_ri && s > 1
                          then
                            (* Root cell shifted toward the seam: a
                                 half-width centred minipage placed in the
                                 seam half of the full-width cell (left half on
                                 the right page, right half on the left page).
                                 Net effect - the root sits at ~1/4 from the
                                 seam edge, i.e. "halfway", not centred under a
                                 single parent. *)
                            let w = colwidth *. Float.of_int s in
                            let fs =
                              if conf.fontsize = "" then ""
                              else "\\" ^ conf.fontsize
                            in
                            let mp_half =
                              Format.sprintf
                                "%s\\begin{minipage}{%1.2f%s}\\begin{center}%s\\linespread{0.80}\\selectfont \
                                 %s%s"
                                fbox_b (w /. 2.0) conf.unit fs str minipage_e
                            in
                            Format.sprintf
                              "\\multicolumn{%d}{c}{\\makebox[%1.2f%s][%s]{%s}}"
                              s w conf.unit
                              (if page = "right" then "l" else "r")
                              mp_half
                          else if s = 1 then
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
                          (* same bridged segments as the hr helper, so
                             full-cell rules also join across the
                             2x\tabcolsep column gaps *)
                          hr s "c"
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
          (* Cut-rule arrow: a horizontal rule that the split truncated at the
             seam was tagged im="cut" in split_tree. Only such a row gets a
             small continuation arrow (-> on the left page, <- on the right),
             so arrows appear exactly where a branch line is severed and
             nowhere else. *)
          let has_cut_rule =
            List.exists
              (fun (_, _, ty, _, _, im) ->
                (ty = "Hl" || ty = "Hr" || ty = "Hc") && im = "cut")
              row
          in
          let row_str =
            if conf.twopages && has_cut_rule then
              if page = "left" then row_str ^ "$\\scriptstyle\\rightarrow$"
              else "$\\scriptstyle\\leftarrow$" ^ row_str
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

  (* A \par before the tree forces it to start on a new line (harmless if we
     are already at the start of one) instead of running on from whatever text
     - a section title, a sentence - precedes it in the document. *)
  "\\par\n"
  ^
  if conf.twopages && !clip_mode && conf.treemode = 1 then (
    (* Clip path: lay out the whole tree once, ~two pages wide, into a save
       box, then show two overlapping clipped windows of it. Each person is
       drawn once at its true position - no split, no truncation, no
       duplication-placement problem. The tabular is built WITHOUT its own
       sideways wrapper (no_sideways_wrap); each window rotates itself via
       adjustbox angle=90. Needs \usepackage{adjustbox}. *)
    let base = if conf.sideways then conf.textheight else conf.textwidth in
    (* Lay the tree out over TWO pages plus the enlarge budget (2 x enl, one
       page's margin each side), minus the overlap the two windows share. The
       wider layout gives every column - so the crowded top row - more room. *)
    let full_w = (2.0 *. (base +. !clip_enlarge)) -. (2.0 *. !clip_overlap) in
    clip_render_width := full_w;
    no_sideways_wrap := true;
    let body = print_tree_mode_1 conf tree "full" in
    no_sideways_wrap := false;
    clip_render_width := 0.0;
    (* Clip with adjustbox's NATIVE trim expression, which supports the box
       keyword \width and calc-style math in braces (per the adjustbox
       manual: trim=0 0 0 {\height-2cm}). This avoids the graphicx number
       parser that appends a "bp" unit and dies on a length register - the
       cause of the earlier "Missing number" errors. `clip` hides the trimmed
       part; `angle=90` rotates for sideways. \width is the pre-rotation width
       (trim runs before angle). Each window keeps half the tree plus
       clip_overlap past the fold, so trim on the far side is
       {0.5\width - overlap}. *)
    (* max width/max totalheight scale a window down if it would overrun the
       page (each window is half the tree plus the overlap, so a touch wider
       than one page) - without it the far edge is clipped by the page
       margin. These are plain adjustbox size keys, unrelated to the trim
       parser that caused the earlier errors. *)
    let rot = if conf.sideways then ",angle=90" else "" in
    let enl = !clip_enlarge in
    (* Raise the width cap by the enlarge amount so a fold-side-extended window
       occupies the margin instead of being scaled back to \textwidth. *)
    let cap =
      Format.sprintf
        ",max width=\\dimexpr\\textwidth+%1.2f%s\\relax,max \
         totalheight=\\textheight"
        enl conf.unit
    in
    let ov = !clip_overlap in
    let u = conf.unit in
    let pl = !clip_pad_l and pr = !clip_pad_r in
    (* shift = pad asymmetry + the user's ClipOffset; the fold (and star) sit
       at 0.5\width + shift, growing the left half and shrinking the right as
       shift increases. *)
    let shift = ((pl -. pr) /. 2.0) +. !clip_fold in
    let sub1 = ov +. shift in
    (* left page: trim right *)
    let sub2 = ov -. shift in
    (* right page: trim left *)
    let hsp off =
      if off = 0.0 then "" else Format.sprintf "\\hspace*{%1.2f%s}" off u
    in
    (* Centre fold markers: re-save the box with a zero-footprint overlay - a
       star at the fold x (0.5\wd + delta), one at the top edge, one at the
       bottom. \makebox[0pt][c] centres it on the fold x with zero width and
       \raisebox[0pt][0pt] gives zero height/depth, so the box dimensions
       (hence the fold math) are unchanged. Both windows include the fold x (it
       is inside the overlap), so both pages show the star at the same tree
       position - a registration cue that reads less like a tree connector
       than a vertical bar did. Star size follows clip_tick. *)
    let star =
      Format.sprintf "\\resizebox{!}{%1.2f%s}{$\\star$}" !clip_tick u
    in
    (* Resolve "auto" from the tree shape: whichever end has fewer people holds
       the root, so put the star at the busier end. *)
    let midpoint =
      if !clip_midpoint <> "auto" then !clip_midpoint
      else
        let ccount row =
          List.fold_left
            (fun a (_, _, ty, _, _, _) ->
              if ty = "Te" || ty = "It" || ty = "Im" then a + 1 else a)
            0 row
        in
        let crows = List.filter (fun r -> classify_row r = Content) tree in
        match crows with
        | [] -> "both"
        | first :: _ ->
            let last = List.nth crows (List.length crows - 1) in
            let cf = ccount first and cl = ccount last in
            if cl < cf then "top" else if cf < cl then "bottom" else "both"
    in
    let want_top = midpoint = "top" || midpoint = "both" in
    let want_bot = midpoint = "bottom" || midpoint = "both" in
    let top_mark =
      Format.sprintf
        "\\makebox[0pt][c]{\\raisebox{\\dimexpr\\ht\\gtree-%1.2f%s\\relax}[0pt][0pt]{%s}}"
        !clip_tick u star
    in
    let bot_mark =
      Format.sprintf "\\makebox[0pt][c]{\\raisebox{-\\dp\\gtree}[0pt][0pt]{%s}}"
        star
    in
    let ticks =
      if !clip_tick <= 0.0 || ((not want_top) && not want_bot) then ""
      else
        Format.sprintf
          "\\savebox\\gtree{\\usebox\\gtree\\kern-0.5\\wd\\gtree\\kern%1.2f%s%s%s\\kern0.5\\wd\\gtree\\kern%1.2f%s}%%\n"
          shift u
          (if want_top then top_mark else "")
          (if want_bot then bot_mark else "")
          (-.shift) u
    in
    let fb, fe = if !show_boxes then ("\\fbox{", "}") else ("", "") in
    Format.sprintf
      "%s\\ifdefined\\gtree\\else\\newsavebox\\gtree\\fi\n\
       \\savebox\\gtree{\\kern%1.2f%s%s\\kern%1.2f%s}%%\n\
       %s\\noindent %s%s\\adjustbox{trim=0pt 0pt {0.5\\width-%1.2f%s} \
       0pt,clip%s%s}{\\usebox\\gtree}%s%%\n\
       \\newpage\n\
       \\noindent %s%s\\adjustbox{trim={0.5\\width-%1.2f%s} 0pt 0pt \
       0pt,clip%s%s}{\\usebox\\gtree}%s%%\n"
      (if !tree_newpage then "\\newpage\n" else "")
      pl u body pr u ticks (hsp !clip_shift_l) fb sub1 u rot cap fe
      (hsp !clip_shift_r) fb sub2 u rot cap fe)
  else if conf.twopages then (
    let tree_left, tree_right = split_tree conf tree in
    test_zero_span_t tree_left "tree_left";
    test_zero_span_t tree_right "tree right";
    match conf.treemode with
    | 0 -> print_tree_mode_0 conf tree_right ^ print_tree_mode_0 conf tree_left
    | 1 ->
        (if conf.debug = 1 then print_tree_mode_0 conf tree_left ^ "\\newpage"
         else "")
        (* Give the first half its own page (see tree_newpage): otherwise a
           section title above it shrinks the usable height and, in sideways
           mode, the rotated half spills off the page. *)
        ^ (if !tree_newpage then "\\newpage\n" else "")
        ^ print_tree_mode_1 conf tree_right "right"
        ^ (if conf.debug = 1 then
             print_tree_mode_0 conf tree_right ^ "\\newpage"
           else if conf.samepage then "\\hspace{40mm}\n"
           else
             (* one half per page: the continuation arrows assume it.
                A \vskip only worked by accident when inflated bar
                heights made each half overflow the page on its own;
                with compact spacing both halves would now share a
                page. *)
             "\\newpage\n")
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
