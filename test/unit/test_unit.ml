let all_test_suites = [ Json_aux.json_suite ]

let _ =
  List.iter
    (fun (suite_name, tests) -> Alcotest.run ("Unit Tests | " ^ suite_name) tests)
    all_test_suites
