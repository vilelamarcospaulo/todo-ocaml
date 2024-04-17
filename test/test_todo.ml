open Lwt.Syntax

let dream_runner ?db_uri =
  let db_uri = Option.value db_uri ~default:"sqlite3:db_test.sqlite" in

  (* TODO :: migrate database *)
  Dream.test @@ Dream.sql_pool db_uri Todo.Http.routes

let test_home _switch () =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  let result = dream_runner @@ request in
  let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in
  let* received_body = Dream.body result in

  Alcotest.(check int "Check the status" 200 received_status);
  Alcotest.(check string) "Check response body" "Todo's API" received_body;

  Lwt.return_unit

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "API /" [ ("/", [ Alcotest_lwt.test_case "index" `Quick test_home ]) ]
