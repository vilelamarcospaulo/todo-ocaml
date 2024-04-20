module type DB = Caqti_lwt.CONNECTION

let connection_string = "sqlite3:db_test.sqlite"
let dream_runner = Dream.test @@ Dream.sql_pool connection_string Todo.Http.routes

type 'a parsed = 'a option
type 'a response = { status : int; body : string; parsed_body : 'a parsed }

let bind (response : 'a response) (f : 'a response -> 'a response) = f response

let finish (response : 'a response) (f : 'a response -> 'a response) =
  let _result = f response in
  ()

let ( >! ) = finish
let ( >!> ) = bind

let given_the_request (req : Dream.request) =
  let result = dream_runner @@ req in
  let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in
  let received_body = Lwt_main.run @@ Dream.body result in

  { status = received_status; body = received_body; parsed_body = None }

let then_the_status_should_be expected response =
  Alcotest.(check int) "Check the status" expected response.status;
  response

let then_the_body_should_be expected response =
  Alcotest.(check string) "Check response body" expected response.body;
  response

let then_with_parsed_body ty_of_yojson response =
  let parsed = response.body |> Yojson.Safe.from_string |> ty_of_yojson in

  { response with parsed_body = Some parsed }

let then_body_should_apply_to (f : 'a -> bool) response =
  let parsed_body = Option.get response.parsed_body in
  Alcotest.(check bool) "Check the parsed body" true (f parsed_body);

  response

let then_db_should_have_n expected (module Db : DB) query_count response =
  let db_count = Lwt_main.run @@ Db.find query_count () in
  let count = match db_count with Ok count -> count | _ -> Alcotest.failf "Error" in

  Alcotest.(check int) "Check the number of items by db_query" expected count;
  response

let then_db_should_not_have_any_items (module Db : DB) query_count response =
  then_db_should_have_n 0 (module Db) query_count response
