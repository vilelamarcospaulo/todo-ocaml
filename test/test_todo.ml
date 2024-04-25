module type DB = Caqti_lwt.CONNECTION

open Flow

let test_home _ =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >! then_the_body_should_be "Todo's API"

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

  Alcotest.run "API /" @@ [ ("/", [ Alcotest.test_case "index" `Quick test_home ]) ] @ api_tests
