open OUnit2

let replace x y str =
  let b = Buffer.create 40 in
  String.iter
    (fun c -> if c = x then Buffer.add_char b y else Buffer.add_char b c)
    str;
  Buffer.contents b

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
  s

let suite =
  [
    "Mutil"
    >::: [
           ( "backslash" >:: fun _ ->
             let test aaa bbb =
               let ccc = clean_double_back_slash bbb in
               if aaa <> ccc then Printf.eprintf "Fail: %s %s\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc def" "abc \\\\def";
             test "abc def" "\\\\abc \\\\def";
             test "abc def" "abc def\\\\" );
           ( "suppress_multiple_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = suppress_multiple_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc def" "abc     def";
             test " abc def" "    abc     def";
             test "abc def " "abc def    " );
           ( "suppress_leading_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = suppress_leading_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc def" "1abc def";
             test "2abc def" "    2abc def";
             test "3abc   def" "3abc   def" );
           ( "suppress_trailing_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = suppress_trailing_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc def" "1abc def    ";
             test "    2abc def" "    2abc def   ";
             test "3abc   def  \n" "3abc   def  \n" );
           ( "replace_utf8_bar" >:: fun _ ->
             let test aaa bbb =
               let ccc = replace_utf8_bar bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             let utf8_bar =
               String.of_seq (List.to_seq [ '\xE2'; '\x94'; '\x82' ])
             in
             test "1abc | def" ("1abc " ^ utf8_bar ^ " def");
             test "2abc |" ("2abc " ^ utf8_bar);
             test "| 3def" (utf8_bar ^ " 3def") );
         ];
  ]
