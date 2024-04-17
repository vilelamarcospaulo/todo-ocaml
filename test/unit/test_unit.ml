let all_test_suits = [ Json_aux.json_suit ]

let _ =
  List.iter
    (fun (suit_name, tests) -> Alcotest.run ("Unit Tests | " ^ suit_name) tests)
    all_test_suits
