open Lwt.Syntax

let test_home _switch () =
  let server = Dream.test Todo.Http.routes in
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  let result = server request in
  let received_status =
    result |> Dream.status |> Dream_pure.Status.status_to_int
  in
  let* received_body = Dream.body result in

  Alcotest.(check int "Check the status" 200 received_status);
  Alcotest.(check string) "Check response body" "Todo's API" received_body;

  Lwt.return_unit

let _ =
  Lwt_main.run
  @@ Alcotest_lwt.run "API /"
       [ ("/", [ Alcotest_lwt.test_case "one" `Quick test_home ]) ]
