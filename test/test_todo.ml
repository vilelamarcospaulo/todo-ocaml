module type DB = Caqti_lwt.CONNECTION

let connection_string = "sqlite3:db_test.sqlite"
let dream_runner = Dream.test @@ Dream.sql_pool connection_string Todo.Http.routes

let test_home _ =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  let result = dream_runner @@ request in
  let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in
  let received_body = Lwt_main.run @@ Dream.body result in

  Alcotest.(check int) "Check the status" 200 received_status;
  Alcotest.(check string) "Check response body" "Todo's API" received_body;

  ()

let test_create_todo_invalid_payload (module Db : DB) () =
  let request = Dream.request ~method_:`POST ~target:"/todos" "{foo}" in

  let result = dream_runner @@ request in
  let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in

  let received_body = Lwt_main.run @@ Dream.body result in

  Alcotest.(check int) "Check the status" 400 received_status;
  Alcotest.(check string) "Check response body" "Invalid request" received_body;

  let db_items = Lwt_main.run @@ Db.collect_list Todo.Item.Q.all () in
  let count = match db_items with Ok items -> List.length items | _ -> Alcotest.failf "Error" in
  Alcotest.(check int) "Check the number of items" 0 count;

  ()

let test_create_todo_successfully (module Db : DB) () =
  let request =
    Dream.request ~method_:`POST ~target:"/todos" "{\"title\": \"foo\", \"description\":\"bar\"}"
  in

  let result = dream_runner @@ request in
  let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in
  let received_body = Lwt_main.run @@ Dream.body result in

  Alcotest.(check int) "Check the status" 200 received_status;
  Alcotest.(check string)
    "Check response body"
    "{\"id\":1,\"title\":\"foo\",\"description\":\"bar\",\"completedAt\":null}" received_body;

  let db_items = Lwt_main.run @@ Db.collect_list Todo.Item.Q.all () in
  let count = match db_items with Ok items -> List.length items | _ -> Alcotest.failf "Error" in
  Alcotest.(check int) "Check the number of items" 1 count;

  ()

let _ =
  Todo.Migration.run connection_string ~down_all:true;

  let db_conn = Lwt_main.run @@ Todo.Migration.conn connection_string in

  Alcotest.run "API /"
    [
      ("/", [ Alcotest.test_case "index" `Quick test_home ]);
      ( "/todo",
        [
          Alcotest.test_case "invalid_payload" `Quick (test_create_todo_invalid_payload db_conn);
          Alcotest.test_case "create_new_item" `Quick (test_create_todo_successfully db_conn);
        ] );
    ]
