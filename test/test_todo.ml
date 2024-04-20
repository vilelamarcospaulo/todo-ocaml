module type DB = Caqti_lwt.CONNECTION

open Flow

let test_home _ =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  given_the_request request
  >!> then_the_status_should_be 200
  >! then_the_body_should_be "Todo's API"

let test_create_todo_invalid_payload (module Db : DB) =
  let request = Dream.request ~method_:`POST ~target:"/todos" "{foo}" in

  given_the_request request
  >!> then_the_status_should_be 400
  >!> then_the_body_should_be "Invalid request"
  >! then_db_should_not_have_any_items (module Db) Todo.Item.Q.all

let test_create_todo_successfully (module Db : DB) =
  let request =
    Dream.request ~method_:`POST ~target:"/todos" "{\"title\": \"foo\", \"description\":\"bar\"}"
  in

  given_the_request request
  >!> then_the_status_should_be 200
  >!> then_the_body_should_be
        "{\"id\":1,\"title\":\"foo\",\"description\":\"bar\",\"completedAt\":null}"
  >!> then_with_parsed_body Todo.Item.t_of_yojson
  >!> then_body_should_apply_to (fun item -> item.Todo.Item.id = 1)
  >! then_db_should_have_n 1 (module Db) Todo.Item.Q.all

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
          Alcotest.test_case "create_new_item1" `Quick (test_setup test_create_todo_successfully);
        ] );
    ]
