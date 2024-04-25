open Todo

let error_testable =
  (module struct
    type t = Server_error.t_error

    let pp formatter _ = Fmt.pf formatter ""
    let _pp_with_index formatter _ = Fmt.pf formatter ""
    let equal h1 h2 = h1 = h2
  end : Alcotest.TESTABLE
    with type t = Server_error.t_error)

let test_from_json () =
  let parsed_json =
    Http.from_json Item.t_new_item_of_yojson "{\"title\": \"foo\", \"description\":\"bar\"}"
  in

  Alcotest.(check bool) "Is a valid Return" true (Result.is_ok parsed_json);
  let item = Result.get_ok parsed_json in
  Alcotest.(check string) "Title is foo" "foo" item.title;
  Alcotest.(check string) "Description is bar" "bar" item.description

let test_from_json_failure () =
  let parsed_json = Http.from_json Todo.Item.t_new_item_of_yojson "foo" in

  Alcotest.(check bool) "Is a invalid Return" true (Result.is_error parsed_json);
  Alcotest.(check error_testable)
    "Is type InvalidJson" Server_error.InvalidJson (Result.get_error parsed_json)

let test_to_json () =
  let (item : Item.t_new_item) = { title = "foo"; description = "bar" } in
  let generated_json = Http.to_json Item.yojson_of_t_new_item item in

  Alcotest.(check string)
    "Is a valid Return" "{\"title\": \"foo\", \"description\":\"bar\"}"
    (Result.get_ok generated_json)

let json_suite =
  ( "JSON aux",
    [
      ( "build strcture from_json",
        [
          Alcotest.test_case "valid str representation" `Quick test_from_json;
          Alcotest.test_case "invalid str representation" `Quick test_from_json_failure;
        ] );
      ( "write structure to_json",
        [ Alcotest.test_case "generate str representation" `Quick test_from_json ] );
    ] )
