module type DB = Caqti_lwt.CONNECTION

open Flow

let test_home _ =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >! then_the_body_should_be "Todo's API"

let test_create_todo_invalid_payload db =
  let request = Dream.request ~method_:`POST ~target:"/todos" "{foo}" in

  given_the_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 400
  >!> then_the_body_should_be "Invalid request"
  >! then_db_should_not_have_any_items Todo.Item.Q.all

let successfully_create_todo_flow todo =
  let body = todo |> Todo.Item.yojson_of_t_new_item |> Yojson.Safe.to_string in
  let request = Dream.request ~method_:`POST ~target:"/todos" body in

  given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body Todo.Item.t_of_yojson

let test_create_todo_successfully db =
  let create_todo : Todo.Item.t_new_item = { title = "foo"; description = "bar" } in
  let given_create_todo_req = successfully_create_todo_flow create_todo in

  let expected_created_item =
    { Todo.Item.id = 1; title = "foo"; description = "bar"; completedAt = None }
  in
  let expected_body = expected_created_item |> Todo.Item.yojson_of_t |> Yojson.Safe.to_string in

  given_create_todo_req
  >!> then_the_body_should_be expected_body
  >!> after_parse_the_body Todo.Item.t_of_yojson
  >!> then_body_should_apply_to (fun item -> item = expected_created_item)
  >!> with_db db
  >! then_db_should_have_n 1 Todo.Item.Q.all

let test_query_unexistend_todo_by_id db =
  let params : Todo.Item.t_new_item = { title = "foo"; description = "bar" } in
  let created_todo = Flow.Hack.extract_item (successfully_create_todo_flow params) in

  let unexistend_id = created_todo.id + 1 in
  let url = "/todos/" ^ string_of_int unexistend_id in
  let request = Dream.request ~method_:`GET ~target:url "" in

  given_the_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 404
  >! then_the_body_should_be "Record not found"

let test_query_todo_by_id db =
  let params : Todo.Item.t_new_item = { title = "foo"; description = "bar" } in
  let created_todo = Flow.Hack.extract_item (successfully_create_todo_flow params) in

  let url = "/todos/" ^ string_of_int created_todo.id in
  let request = Dream.request ~method_:`GET ~target:url "" in

  let expected_body = created_todo |> Todo.Item.yojson_of_t |> Yojson.Safe.to_string in

  given_the_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >! then_the_body_should_be expected_body

let test_with_db db_conn f () =
  (* TODO :: Open a connection with different db per execution id
     e.g. connection_string = "sqlite3:db_test_$execution_id.sqlite"
     this will enable to run tests in parallel
  *)
  Todo.Migration.run db_conn ~down_all:true;
  f db_conn

let _ =
  let connection_string = "sqlite3:db_test.sqlite" in
  let db_conn = Lwt_main.run @@ Todo.Migration.conn connection_string in
  let test_setup = test_with_db db_conn in

  Alcotest.run "API /"
    [
      ("/", [ Alcotest.test_case "index" `Quick test_home ]);
      ( "/todo",
        [
          Alcotest.test_case "invalid_payload" `Quick
            (test_setup @@ test_create_todo_invalid_payload);
          Alcotest.test_case "create_new_item" `Quick (test_setup test_create_todo_successfully);
          Alcotest.test_case "fetch_item_by_id" `Quick (test_setup test_query_todo_by_id);
        ] );
      ( "/todo/{id}",
        [
          Alcotest.test_case "fetch_unexistend_item_by_id" `Quick
            (test_setup test_query_unexistend_todo_by_id);
          Alcotest.test_case "fetch_item_by_id" `Quick (test_setup test_query_todo_by_id);
        ] );
    ]
