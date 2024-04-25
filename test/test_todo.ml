module type DB = Caqti_lwt.CONNECTION

open Flow

let test_home _ =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >! then_the_body_should_be "Todo's API"

let create_todo_flow _ =
  let item : Todo.Item.t_new_item = { title = "foo"; description = "bar" } in
  let body = item |> Todo.Item.yojson_of_t_new_item |> Yojson.Safe.to_string in
  let request = Dream.request ~method_:`POST ~target:"/todos" body in

  given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body Todo.Item.t_of_yojson
  >!> then_body_should_apply_to (fun (item : Todo.Item.t) ->
          item.title = "foo" && item.description = "bar")

let test_delete_unexistend db =
  let request = Dream.request ~method_:`DELETE ~target:"/todos/2" "" in

  create_todo_flow ()
  >!> then_given_a_new_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 404
  >!> then_the_body_should_be "Record not found"

let test_delete_by_id db =
  let first_item = Flow.Hack.extract_item @@ create_todo_flow db in

  let url = "/todos/" ^ string_of_int first_item.id in

  let request = Dream.request ~method_:`DELETE ~target:url "" in
  let expected_body = first_item |> Todo.Item.yojson_of_t |> Yojson.Safe.to_string in

  create_todo_flow ()
  >!> then_given_a_new_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body Todo.Item.t_of_yojson
  >!> then_the_body_should_be expected_body
  >!> with_db db
  >!> then_db_should_have_n 1 Todo.Item.Q.all

let flow_exec_with_db db_conn f () =
  (* TODO :: Open a connection with different db per execution id
     e.g. connection_string = "sqlite3:db_test_$execution_id.sqlite"
     this will enable to run tests in parallel
  *)
  Todo.Migration.run db_conn ~down_all:true;
  let _flow_result = f db_conn in
  ()

let flows_to_alcotest flow_to_test suite =
  let title, flows = suite in
  let tests =
    List.map
      (fun (test_title, test_flow) ->
        let test = flow_to_test test_flow in
        Alcotest.test_case test_title `Quick test)
      flows
  in

  (title, tests)

let _ =
  let connection_string = "sqlite3:db_test.sqlite" in
  let db_conn = Lwt_main.run @@ Todo.Migration.conn connection_string in
  let flow_executor = flow_exec_with_db db_conn in

  let api_tests = List.map (flows_to_alcotest flow_executor) Test_api.test_api_suites in

  Alcotest.run "API /"
  @@ [ ("/", [ Alcotest.test_case "index" `Quick test_home ]) ]
  @ api_tests
  @ [
      ( "/todo/{id}",
        [
          Alcotest.test_case "delete_unexistend" `Quick (flow_executor test_delete_unexistend);
          Alcotest.test_case "delete_item_by_id" `Quick (flow_executor test_delete_by_id);
        ] );
    ]
