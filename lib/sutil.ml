(* strings utilities *)
(* v1  Henri, 2023/10/16 *)

let version = "1.0"

let partics =
  [
    "d'";
    "d‘";
    "d’";
    "Dr ";
    "M. ";
    "Mr ";
    "Mme ";
    "de ";
    "di ";
    "du ";
    "van ";
    "von ";
  ]

let apostr = [ "d'"; "d‘"; "d’" ]
(* TODO expand partics, make it a parameter *)

let start_with ini i s =
  let inilen = String.length ini in
  let strlen = String.length s in
  if i < 0 || i > strlen then raise (Invalid_argument "start_with");
  let rec loop i1 i2 =
    if i1 = inilen then true
    else if i2 = strlen then false
    else if String.unsafe_get s i2 = String.unsafe_get ini i1 then
      loop (i1 + 1) (i2 + 1)
    else false
  in
  loop 0 i

let particles sn =
  let rec loop partics =
    match partics with
    | [] -> sn
    | p :: partics ->
        if start_with p 0 sn then
          (* TODO test for fancy apostrophs as well *)
          let len = String.length p in
          if String.length sn > len then
            String.sub sn len (String.length sn - len)
            ^ Format.sprintf " (%s)" (String.trim p)
          else sn
        else loop partics
  in
  loop partics

(** Read a line. If line is empty or only contains a comment (#), then read next line  *)
let rec input_real_line ic =
  let x = input_line ic in
  if String.length x > 0 && x.[0] = '#' then input_real_line ic else x

let read_line ic =
  try
    let str = input_real_line ic in
    Some str
  with End_of_file -> None

(* convert &#xx; html notation *)
let convert_html str =
  let rec loop str1 str2 =
    if String.length str1 = 0 then str2 ^ str1
    else
      let j = try String.index str1 '&' with Not_found -> -1 in
      if j = -1 then str2 ^ str1
      else
        let k = try String.index_from str1 j ';' with Not_found -> -1 in
        if k = -1 then str2 ^ str1
        else
          let char = String.sub str1 j (k - j + 1) in
          let new_char = match char with "&#38;" -> "&" | _ -> char in
          loop
            (if k < String.length str1 then
             String.sub str1 (k + 1) (String.length str1 - k - 1)
            else "")
            (String.sub str1 0 j ^ str2 ^ new_char)
  in
  loop str ""

let encode s =
  let special = function
    | '\000' .. '\031'
    | '\127' .. '\255'
    | '<' | '>' | '"' | '#' | '%' | '{' | '}' | '|' | '\\' | '^' | '~' | '['
    | ']' | '`' | ';' | '/' | '?' | ':' | '@' | '=' | '&' | '+' ->
        true
    | _ -> false
  in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'A' + x - 10)
    else Char.chr (Char.code '0' + x)
  in
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      | ' ' -> true
      | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        | ' ' ->
            Bytes.set s1 i1 '+';
            succ i1
        | c ->
            if special c then (
              Bytes.set s1 i1 '%';
              Bytes.set s1 (i1 + 1) (hexa_digit (Char.code c / 16));
              Bytes.set s1 (i1 + 2) (hexa_digit (Char.code c mod 16));
              i1 + 3)
            else (
              Bytes.set s1 i1 c;
              succ i1)
      in
      copy_code_in s1 (succ i) i1
    else Bytes.unsafe_to_string s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in
    copy_code_in (Bytes.create len) 0 0
  else s

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

let contains_index str sub =
  let strlen = String.length str in
  let sublen = String.length sub in
  let rec aux i1 i2 =
    if i1 = sublen then i2 - sublen
    else if i2 = strlen then -1
    else if String.unsafe_get str i2 = String.unsafe_get sub i1 then
      aux (i1 + 1) (i2 + 1)
    else -1
  in
  let rec loop i =
    if i + sublen <= strlen then
      let r = aux 0 i in
      if r <> -1 then r else loop (i + 1)
    else -1
  in
  loop 0

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

(* in str, replace car x by car y *)
let replace x y str =
  let b = Buffer.create 40 in
  String.iter
    (fun c -> if c = x then Buffer.add_char b y else Buffer.add_char b c)
    str;
  Buffer.contents b

let replace_str2 line sub1 sub2 =
  if String.length sub1 > 0 && String.length line > 0 then
    Str.global_replace (Str.regexp sub1) sub2 line
  else line

let replace_str sub1 sub2 line =
  if String.length sub1 > 0 && String.length line > 0 then
    let i = try String.index line sub1.[0] with Not_found -> -1 in
    if i >= 0 && String.length line > String.length sub1 then
      let yes =
        let rec loop ok j =
          if j = String.length sub1 then true
          else if i + j = String.length line then false
          else if sub1.[j] <> line.[i + j] then false
          else loop ok (j + 1)
        in
        loop true 0
      in
      if yes then
        String.sub line 0 i ^ sub2
        ^ String.sub line
            (i + String.length sub1)
            (String.length line - i - String.length sub1)
      else line
    else line
  else line

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
  if str <> "" then
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
  else str

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

(** Removes spaces at the begining and at the end of string. *)
let cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len - 1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)

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

let clean_double_back_slash_2 str =
  let str =
    replace '\n' ' ' str |> suppress_trailing_sp |> suppress_multiple_sp
  in
  let str =
    let rec loop j s =
      let i = try String.index_from s j '\\' with Not_found -> -1 in
      if i = -1 then s
      else if i >= 0 && String.length s > i + 4 then
        if
          s.[i + 1] = '\\'
          && s.[i + 2] = ' '
          && s.[i + 3] = '\\'
          && s.[i + 4] = '\\'
        then
          loop i
            (String.sub s 0 i ^ "\\\\"
            ^
            if String.length s > i + 2 then
              String.sub s (i + 5) (String.length s - i - 5)
            else "")
        else loop (i + 1) s
      else s
    in
    loop 0 str
  in
  str

let clean_leading_double_back_slash str =
  let i = try String.index str '\\' with Not_found -> -1 in
  if i = 0 && String.length str > i + 1 && str.[i + 1] = '\\' then
    String.sub str 0 i
    ^
    if String.length str > i + 2 then
      String.sub str (i + 2) (String.length str - i - 2)
    else ""
  else str

let clean_double_back_slash str =
  let str =
    let rec loop j s =
      let i = try String.index_from s j '\\' with Not_found -> -1 in
      if i = -1 then s
      else if i >= 0 && String.length s > i + 1 then
        if s.[i + 1] = '\\' then
          loop i
            (String.sub s 0 i
            ^
            if String.length s > i + 2 then
              String.sub s (i + 2) (String.length s - i - 2)
            else "")
        else loop (i + 1) s
      else s
    in
    loop 0 str
  in
  str

let clean_item str =
  replace '\n' ' ' str |> suppress_multiple_sp |> suppress_leading_sp
  |> suppress_trailing_sp

let unaccent_utf8 lower s i =
  let fns =
    if lower then fun n s -> (String.lowercase_ascii s, n) else fun n s -> (s, n)
  in
  let fnc =
    if lower then fun n c -> (String.make 1 @@ Char.lowercase_ascii c, n)
    else fun n c -> (String.make 1 c, n)
  in
  let s, n =
    Unidecode.decode fns fnc
      (fun n -> (String.sub s i (n - i), n))
      s i (String.length s)
  in
  if lower then (String.lowercase_ascii s, n) else (s, n)

let lower s =
  let rec copy special i len =
    if i = String.length s then Buff.get len
    else if Char.code s.[i] < 0x80 then
      match s.[i] with
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_') as c ->
          let len = if special then Buff.store len ' ' else len in
          let c = Char.lowercase_ascii c in
          copy false (i + 1) (Buff.store len c)
      | _ -> copy (len <> 0) (i + 1) len
    else
      let len = if special then Buff.store len ' ' else len in
      let t, j = unaccent_utf8 true s i in
      copy false j (Buff.mstore len t)
  in
  copy false 0 0
