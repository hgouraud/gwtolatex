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
