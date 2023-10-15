open Geneweb
open OUnit2

let backslash _ =
  let test aaa bbb = 
    let ccc = clean_double_back_slash bbb in
    assert aaa = ccc
  in
  test "abc def" "abc \\\\def";
  test "abc def\\\\"

let suite =
  [
    "Mutil"
    >::: [
           "mutil_contains" >:: mutil_contains;
         ];
  ]
