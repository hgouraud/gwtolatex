open OUnit2
open Gwtolatex

let suite =
  [
    "Mutil"
    >::: [
           ( "backslash" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.clean_double_back_slash bbb in
               if aaa <> ccc then Printf.eprintf "Fail: %s %s\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc def" "abc \\\\def";
             test "abc def" "\\\\abc \\\\def";
             test "abc def" "abc def\\\\" );
           ( "suppress_multiple_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.suppress_multiple_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc def" "abc     def";
             test " abc def" "    abc     def";
             test "abc def " "abc def    " );
           ( "suppress_leading_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.suppress_leading_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc def" "1abc def";
             test "2abc def" "    2abc def";
             test "3abc   def" "3abc   def" );
           ( "suppress_trailing_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.suppress_trailing_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc def" "1abc def    ";
             test "    2abc def" "    2abc def   ";
             test "3abc   def  \n" "3abc   def  \n" );
           ( "replace_utf8_bar" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.replace_utf8_bar bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             let utf8_bar =
               String.of_seq (List.to_seq [ '\xE2'; '\x94'; '\x82' ])
             in
             test "1abc | def" ("1abc " ^ utf8_bar ^ " def");
             test "2abc |" ("2abc " ^ utf8_bar);
             test "| 3def" (utf8_bar ^ " 3def") );
           ( "replace_str" >:: fun _ ->
             let test aaa bbb sub1 sub2 =
               let ccc = Sutil.replace_str bbb sub1 sub2 in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc xyz ghi" "1abc def ghi" "def" "xyz";
             test "1abc def ghi" "1abc def ghi" "" "xyz";
             test "" "" "def" "";
             test "abc%" "abc%" "%%%" "xyz";
             test "1abc  ghi" "1abc def ghi" "def" "";
             test "xyz 2ghi" "def 2ghi" "def" "xyz";
             test "3abc xyz" "3abc def" "def" "xyz" );
         ];
  ]
