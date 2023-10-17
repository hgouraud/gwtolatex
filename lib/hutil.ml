(* html utilities *)
(* v1  Henri, 2023/10/16 *)

let test_attr attributes attr value =
  List.exists (fun ((_, k), v) -> k = attr && v = value) attributes

let get_attr attributes attr =
  List.fold_left
    (fun c ((_, k), v) -> if k = attr then v ^ c else c)
    "" attributes
