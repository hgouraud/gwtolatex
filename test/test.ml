open OUnit2

let _ = run_test_tt_main ("GwToLaTeX" >::: [] @ Test_gwtolatex.suite)
