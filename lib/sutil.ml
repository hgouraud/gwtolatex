(* strings utilities *)
(* v1  Henri, 2023/10/16 *)

(* convert %xx utf-8 notation *)
let decode s =
  let hexa_val conf =
    match conf with
    | '0' .. '9' -> Char.code conf - Char.code '0'
    | 'a' .. 'f' -> Char.code conf - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code conf - Char.code 'A' + 10
    | _ -> 0
  in
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with '%' -> true | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        | '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
        | '%' when i + 2 < String.length s ->
            let v = (hexa_val s.[i + 1] * 16) + hexa_val s.[i + 2] in
            Bytes.set s1 i1 (Char.chr v);
            i + 3
        | x ->
            Bytes.set s1 i1 x;
            succ i
      in
      copy_decode_in s1 i (succ i1)
    else Bytes.unsafe_to_string s1
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    let s = copy_decode_in s1 0 0 in
    s
  else s

(* in str, replace car x by car y *)
let replace x y str =
  let b = Buffer.create 40 in
  String.iter
    (fun c -> if c = x then Buffer.add_char b y else Buffer.add_char b c)
    str;
  Buffer.contents b

let contains str sub =
  let strlen = String.length str in
  let sublen = String.length sub in
  let rec aux i1 i2 =
    if i1 = sublen then true
    else if i2 = strlen then false
    else if String.unsafe_get str i2 = String.unsafe_get sub i1 then
      aux (i1 + 1) (i2 + 1)
    else false
  in
  let rec loop i =
    if i + sublen <= strlen then aux 0 i || loop (i + 1) else false
  in
  loop 0

let suppress_multiple_sp str =
  let b = Buffer.create 100 in
  let rec loop cond i =
    if i = String.length str then Buffer.contents b
    else if str.[i] <> ' ' then (
      Buffer.add_char b str.[i];
      loop true (i + 1))
    else if cond then (
      Buffer.add_char b str.[i];
      loop false (i + 1))
    else loop false (i + 1)
  in
  loop true 0

let suppress_trailing_sp str =
  let b = Buffer.create 100 in
  let rec loop cond i =
    if i = 0 then (
      Buffer.add_char b str.[i];
      Buffer.to_seq b |> List.of_seq |> List.rev
      |> List.map (fun c -> String.make 1 c)
      |> String.concat "")
    else if str.[i] = ' ' && cond then loop cond (i - 1)
    else (
      Buffer.add_char b str.[i];
      loop false (i - 1))
  in
  loop true (String.length str - 1)

let suppress_leading_sp str =
  let b = Buffer.create 100 in
  let rec loop cond i =
    if i = String.length str then Buffer.contents b
    else if str.[i] = ' ' && cond then loop cond (i + 1)
    else (
      Buffer.add_char b str.[i];
      loop false (i + 1))
  in
  loop true 0

let replace_utf8_bar str =
  let rec loop s =
    let i = try String.index s '\xE2' with Not_found -> -1 in
    if i = -1 then s
    else if i >= 0 && String.length s > i + 2 then
      if String.length s > i + 2 && s.[i + 1] = '\x94' && s.[i + 2] = '\x82'
      then
        loop
          ((if i = 0 then "" else String.sub s 0 i)
          ^ "|"
          ^
          if String.length s > i + 3 then
            String.sub s (i + 3) (String.length s - i - 3)
          else "")
      else s
    else s
  in
  loop str

let clean_double_back_slash str =
  let s =
    let rec loop s =
      let i = try String.index s '\\' with Not_found -> -1 in
      if i = -1 then s
      else if i >= 0 && String.length s > i + 1 then
        if s.[i + 1] = '\\' then
          loop
            (String.sub s 0 i
            ^
            if String.length s > i + 2 then
              String.sub s (i + 2) (String.length s - i - 2)
            else "")
        else s
      else s
    in
    loop str
  in
  let s = replace '\n' ' ' s in
  suppress_multiple_sp s

