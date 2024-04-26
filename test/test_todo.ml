open Flow

let test_home setup =
  let request = Dream.request ~method_:`GET ~target:"/" "" in

  setup
  >!> given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> then_the_body_should_be "Todo's API"

let flow_executor flow () =
  (* TODO :: Not sure if random flow_id is a good idea, but for now it's fine
     maybe use the alcotest seed to make it deterministic *)
  let flow_id = Random.int 1000 in
  let config = Sysmap.test_config (string_of_int flow_id) in
  let sysmap = Sysmap.init config in

  let conn = sysmap.sqlconn in
  Todo.Migration.run conn ~down_all:true;

  let dream_runner = Dream.test @@ Dream.sql_pool config.db_uri @@ Todo.Http.routes in
  let flow_setup = setup dream_runner conn in

  let _result = flow flow_setup in
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
  let api_tests = List.map (flows_to_alcotest flow_executor) Test_api.test_api_suites in

  Alcotest.run "API /"
  @@ [ ("/", [ Alcotest.test_case "index" `Quick (flow_executor test_home) ]) ]
  @ api_tests
