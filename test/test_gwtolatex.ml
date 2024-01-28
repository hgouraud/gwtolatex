open OUnit2
open Gwtolatex

let suite =
  [
    "Mutil"
    >::: [
           ( "backslash" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.clean_double_back_slash bbb in
               if aaa <> ccc then Printf.eprintf "Fail: (%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc def" "abc \\\\def";
             test "abc def" "\\\\abc \\\\def";
             test "abc def" "abc def\\\\";
             test "" "\\\\";
             test "\\" "\\";
             test "\n" "\\\\\n";
             test "" "";
             test "" "\\\\";
             test "\\" "\\";
             test "\n" "\\\\\n";
             test "" "";
             test "\\&{}1968 Sylvie" "\\&{}1968 Sylvie";
             test "\\&{}1968  Sylvie1 Sautin 1946  "
               "\\&{}1968 \\\\ Sylvie1 Sautin 1946 \\\\ \\\\";
             test "\\&{}1968  Sylvie2 Sautin 1946  "
               "\\&{}1968 \\\\ Sylvie2 Sautin 1946 \\\\ \\\\" );
           ( "backslash 2" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.clean_double_back_slash_2 bbb in
               if aaa <> ccc then Printf.eprintf "Fail: (%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "\\&{}1968 \\\\ Sylvie1 Sautin 1946 \\\\"
               "\\&{}1968 \\\\ Sylvie1 Sautin 1946 \\\\ \\\\";
             test "\\&{}1968 \\\\ Sylvie2 Sautin 1946 \\\\xx"
               "\\&{}1968 \\\\ Sylvie2 Sautin 1946 \\\\ \\\\xx";
             test "\\&{}1968 \\\\ Sylvie3 Sautin 1946 \\\\"
               "\\&{}1968 \\\\ Sylvie3 Sautin 1946 \\\\ \\\\\n";
             test "" "" );
           ( "backslash 3" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.clean_leading_double_back_slash bbb in
               if aaa <> ccc then Printf.eprintf "Fail: (%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "aaa" "\\\\aaa";
             test "bbb" "bbb";
             test "" "" );
           ( "suppress_multiple_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.suppress_multiple_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc def" "abc     def";
             test " abc def" "    abc     def";
             test "abc def " "abc def    ";
             test "" "" );
           ( "get item length" >:: fun _ ->
             let test aaa bbb =
               let ccc = Trees.get_item_length bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%d) (%d)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test 8 "1abc def";
             test 5 "2abc \\\\ def";
             test 7 "2abcxx \\\\ def";
             test 7 "2abc \\\\ 2defxx\\\\";
             test 11 "2abc \\\\ defxx\\\\abcdefghijk";
             test 12 "2abc \\\\ abcdefghijk\\\\abc";
             test 10 "3abc   def\\\\";
             test 0 "" );
           ( "suppress_leading_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.suppress_leading_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc def" "1abc def";
             test "2abc def" "    2abc def";
             test "3abc   def" "3abc   def";
             test "" "" );
           ( "suppress_trailing_sp" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.suppress_trailing_sp bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc def" "1abc def    ";
             test "    2abc def" "    2abc def   ";
             test "3abc   def  \n" "3abc   def  \n";
             test "" "" );
           ( "replace_str" >:: fun _ ->
             let test aaa bbb sub1 sub2 =
               let ccc = Sutil.replace_str sub1 sub2 bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc xyz ghi" "1abc def ghi" "def" "xyz";
             test "1abc def ghi" "1abc def ghi" "" "xyz";
             test "" "" "def" "";
             test "abc%" "abc%" "%%%" "xyz";
             test "1abc  ghi" "1abc def ghi" "def" "";
             test "xyz 2ghi" "def 2ghi" "def" "xyz";
             test "3abc xyz" "3abc def" "def" "xyz";
             test "4abc_def" "4abc\\_{}def" "\\_{}" "_";
             test "5abc  def" "5abc xyz def" "xyz" "";
             test "6abc xyz def" "6abc xyz def" "" "xx";
             test "" "" "" "" );
           ( "replace_str2" >:: fun _ ->
             let test aaa bbb sub1 sub2 =
               let ccc = Sutil.replace_str2 sub1 sub2 bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "1abc xyz ghi" "1abc def ghi" "def" "xyz";
             test "1abc def ghi" "1abc def ghi" "" "xyz";
             test "" "" "def" "";
             test "abc%" "abc%" "%%%" "xyz";
             test "1abc  ghi" "1abc def ghi" "def" "";
             test "xyz 2ghi" "def 2ghi" "def" "xyz";
             test "3abc xyz" "3abc def" "def" "xyz";
             (*test "4abc_def" "4abc\\_{}def" "\\_{}" "_";*)
             test "5abc  def" "5abc xyz def" "xyz" "";
             test "6abc xyz def" "6abc xyz def" "" "xx";
             test "" "" "" "" );
           ( "particles" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.particles bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "Charnacé (de)" "de Charnacé";
             test "Gouraud" "Gouraud";
             test "" "" );
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
           ( "lower" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.lower bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "benedicte.0.gouraud" "Bénédicte.0.Gouraud";
             test "andre.0.fauchon-villeplee" "André.0.Fauchon-Villeplée";
             test "andre.0.fauchon_villeplee" "André.0.Fauchon_Villeplée";
             test "" "" );
           ( "contains" >:: fun _ ->
             let test aaa bbb sss =
               let ccc = Sutil.contains bbb sss in
               assert (aaa = ccc)
             in
             test true "aaaedicte" "aaa";
             test true "bbbaaacccccccc" "aaa";
             test true "bbbbaaa" "aaa";
             test false "bbbbaaa" "aaaa";
             test true "" "" );
           ( "contains_index" >:: fun _ ->
             let test aaa bbb sss =
               let ccc = Sutil.contains_index bbb sss in
               if aaa <> ccc then Printf.eprintf "Fail:(%d) (%d)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test 0 "aaaedicte" "aaa";
             test 3 "bbbaaacccccccc" "aaa";
             test 4 "bbbbaaa" "aaa";
             test 0 "" "" );
           ( "escape" >:: fun _ ->
             let test aaa bbb =
               let ccc = Lutil.escape bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "aaaedicte" "aaaedicte";
             test "aaa\\_{}edicte" "aaa_edicte";
             test "aaa\\includegraphics{xx_xx}bbb"
               "aaa\\includegraphics{xx_xx}bbb";
             test "\\includegraphics{xx_xx}" "\\includegraphics{xx_xx}";
             test "\\includegraphics{xxxx}" "\\includegraphics{xxxx}";
             test "aaa{b\\_{}b}\\includegraphics{xxxx}"
               "aaa{b_b}\\includegraphics{xxxx}";
             test "\\includegraphics{xx_xx}\n" "\\includegraphics{xx_xx}\n";
             test "" "" );
           ( "get_nb_full_col_in_span" >:: fun _ ->
             let cols =
               [
                 "E1";
                 "E2";
                 "E3";
                 "F4";
                 "F5";
                 "F6";
                 "E7";
                 "E8";
                 "F9";
                 "F10";
                 "F11";
                 "E12";
               ]
             in
             let test l res b n =
               let ccc = Trees.get_nb_full_col_in_span cols b n in
               if ccc <> res then
                 Printf.eprintf "Fail: %s res: (%d) (%d)\n" l ccc res;
               assert (ccc = res)
             in
             test "a" 0 0 1;
             test "b" 0 0 2;
             test "b" 0 0 3;
             test "c" 1 0 4;
             test "d" 2 3 2 );
           ( "is_empty_cell" >:: fun _ ->
             let row =
               [
                 (0, 1, "E", "1", "", "");
                 (0, 2, "E", "2", "", "");
                 (0, 3, "It", "4", "", "");
                 (0, 1, "E", "7", "", "");
                 (0, 1, "E", "8", "", "");
                 (0, 3, "It", "9", "", "");
                 (0, 1, "E", "12", "", "");
               ]
             in
             let test res n =
               let ccc = Trees.is_empty_cell row n in
               assert (ccc = res)
             in
             test true 1;
             test true 2;
             test true 3;
             test false 4;
             test true 8;
             test false 10;
             test true 12;
             test false 14 );
           ( "find_empty_columns" >:: fun _ ->
             let row1 =
               [
                 (0, 1, "E", "1", "", "");
                 (0, 2, "E", "2", "", "");
                 (0, 3, "It", "4", "", "");
                 (0, 1, "E", "7", "", "");
                 (0, 1, "E", "8", "", "");
                 (0, 3, "It", "9", "", "");
                 (0, 1, "E", "12", "", "");
               ]
             in
             let row2 =
               [
                 (0, 1, "E", "1", "", "");
                 (0, 2, "E", "2", "", "");
                 (0, 3, "It", "4", "", "");
                 (0, 1, "E", "7", "", "");
                 (0, 1, "E", "8", "", "");
                 (0, 3, "It", "9", "", "");
                 (0, 1, "E", "12", "", "");
               ]
             in
             let row3 =
               [
                 (0, 1, "E", "1", "", "");
                 (0, 2, "E", "2", "", "");
                 (0, 3, "It", "4", "", "");
                 (0, 1, "It", "7", "", "");
                 (0, 1, "It", "8", "", "");
                 (0, 3, "It", "9", "", "");
                 (0, 1, "E", "12", "", "");
               ]
             in
             let my_tree = [ row1; row2 ] in
             let cols1 =
               [
                 "E1";
                 "E2";
                 "E3";
                 "F4";
                 "F5";
                 "F6";
                 "E7";
                 "E8";
                 "F9";
                 "F10";
                 "F11";
                 "E12";
               ]
             in
             let cols2 =
               [
                 "E1";
                 "E2";
                 "E3";
                 "F4";
                 "F5";
                 "F6";
                 "F7";
                 "F8";
                 "F9";
                 "F10";
                 "F11";
                 "E12";
               ]
             in
             let test res one_tree =
               let ccc = Trees.find_empty_columns one_tree in
               if ccc <> res then
                 List.iter (fun c -> Printf.eprintf "%s, " c) ccc;
               assert (ccc = res)
             in
             test cols1 my_tree;
             let my_tree2 = [ row1; row3 ] in
             test cols2 my_tree2 );
           ( "convert_html" >:: fun _ ->
             let test aaa bbb =
               let ccc = Sutil.convert_html bbb in
               if aaa <> ccc then Printf.eprintf "Fail:(%s) (%s)\n" aaa ccc;
               assert (aaa = ccc)
             in
             test "abc&def" "abc&#38;def";
             test "abc&" "abc&#38;";
             test "abc&#38" "abc&#38";
             test "&def" "&#38;def";
             test "" "" );
         ];
  ]
