(* Copyright (c) 2013 H.Gouraud *)

type name = string * string

(* TODO suppress (pages liées) and (modifier) in m=NOTES *)
(* TODO suppress "base chausey ..." *)
type my_tree =
  | Text of string
  | Element of string * (name * string) list * my_tree list

(* TODO Imagek = Portrait ?? no *)
type im_type = Portrait | Imagek | Images | Vignette

type image = {
  im_type : im_type;
  filename : string;
  where : int * int * int * int; (* ch, sec, ssec, sssec*)
  nbr : int;
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

type config = {
  bases : string;
  basename : string;
  passwd : string;
  family : string;
  debug : int;
  verbose : bool;
  treemode : int;
  (* formatting *)
  unit : string;
  textwidth : float;
  textheight : float;
  margin : float;
  colsep : float;
  rulethickns : float;
  fontsize : string;
  imgwidth : float;
  vignwidth : float;
  portraitwidth : float;
  sideways : bool;
  twopages : bool;
  double : bool;
  split : int;
  (* mkTex *)
  arbres : bool;
  sub : bool;
  collectimages : bool;
  sectiononatag : bool;
  highlights : string list;
  hrule : bool;
  imagelabels : int;
  nbimgperline : int;
  offset : bool;
  wide : bool;
  hoffset : float;
  voffset : float;
}
