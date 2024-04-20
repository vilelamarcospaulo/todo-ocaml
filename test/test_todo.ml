module type DB = Caqti_lwt.CONNECTION

let connection_string = "sqlite3:db_test.sqlite"
let dream_runner = Dream.test @@ Dream.sql_pool connection_string Todo.Http.routes

module BddTestFlow = struct
  type response = { status : int; body : string }

  let bind (response : response) (f : response -> response) = f response

  let finish (response : response) (f : response -> response) =
    let _result = f response in
    ()

  let ( >! ) = finish
  let ( >!> ) = bind

  let given_the_request (req : Dream.request) =
    let result = dream_runner @@ req in
    let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in
    let received_body = Lwt_main.run @@ Dream.body result in

    { status = received_status; body = received_body }

  let then_the_status_should_be (expected : int) (response : response) =
    Alcotest.(check int) "Check the status" expected response.status;
    response

  let then_the_body_should_be (expected : string) (response : response) =
    Alcotest.(check string) "Check response body" expected response.body;
    response
end

open BddTestFlow

let test_home _ =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  given_the_request request
  >!> then_the_status_should_be 200
  >! then_the_body_should_be "Todo's API"

let test_create_todo_invalid_payload (module Db : DB) () =
  let request = Dream.request ~method_:`POST ~target:"/todos" "{foo}" in

  given_the_request request
  >!> then_the_status_should_be 400
  >! then_the_body_should_be "Invalid request";

  let db_items = Lwt_main.run @@ Db.collect_list Todo.Item.Q.all () in
  let count = match db_items with Ok items -> List.length items | _ -> Alcotest.failf "Error" in
  Alcotest.(check int) "Check the number of items" 0 count;

  ()

let test_create_todo_successfully (module Db : DB) () =
  let request =
    Dream.request ~method_:`POST ~target:"/todos" "{\"title\": \"foo\", \"description\":\"bar\"}"
  in

  given_the_request request
  >!> then_the_status_should_be 200
  >! then_the_body_should_be
       "{\"id\":1,\"title\":\"foo\",\"description\":\"bar\",\"completedAt\":null}";

  let db_items = Lwt_main.run @@ Db.collect_list Todo.Item.Q.all () in
  let count = match db_items with Ok items -> List.length items | _ -> Alcotest.failf "Error" in
  Alcotest.(check int) "Check the number of items" 1 count;

  ()

let _ =
  let db_conn = Lwt_main.run @@ Todo.Migration.conn connection_string in
  Todo.Migration.run db_conn ~down_all:true;

  Alcotest.run "API /"
    [
      ("/", [ Alcotest.test_case "index" `Quick test_home ]);
      ( "/todo",
        [
          Alcotest.test_case "invalid_payload" `Quick (test_create_todo_invalid_payload db_conn);
          Alcotest.test_case "create_new_item" `Quick (test_create_todo_successfully db_conn);
        ] );
    ]
