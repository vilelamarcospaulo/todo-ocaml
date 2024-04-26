module type DB = Caqti_lwt.CONNECTION

type 'a response = { status : int; body : string; parsed_body : 'a option }
type request = Dream.request
type runner = Dream.request -> Dream.response

type 'a context = {
  runner : runner;
  db : (module DB);
  request : request option;
  response : 'a response option;
}

type 'a step = 'a context -> 'a context

let bind (ctx : 'a context) (f : 'a step) = f ctx
let ( >!> ) = bind

(**)
let setup runner db = { runner; db; request = None; response = None }
let given_the_request request context = { context with request = Some request }

let then_follow_a_new_request (req_builder : 'a context -> request) context =
  let new_request = req_builder context in
  { context with request = Some new_request; response = None }

let when_the_request_is_sent context =
  let result = context.runner (Option.get context.request) in
  let received_status = result |> Dream.status |> Dream_pure.Status.status_to_int in
  let received_body = Lwt_main.run @@ Dream.body result in

  let response = { status = received_status; body = received_body; parsed_body = None } in

  { context with response = Some response }

let then_the_status_should_be expected context =
  let response = Option.get context.response in

  Alcotest.(check int) "Check the status" expected response.status;
  context

let then_the_body_should_be expected context =
  let response = Option.get context.response in

  Alcotest.(check string) "Check response body" expected response.body;
  context

let then_the_body_should_be_lazy (expect : string ref) context =
  then_the_body_should_be !expect context

let after_parse_the_body ty_of_yojson context =
  let response = Option.get context.response in
  let parsed = response.body |> Yojson.Safe.from_string |> ty_of_yojson in

  let response = { response with parsed_body = Some parsed } in
  { context with response = Some response }

let then_body_should_apply_to (f : 'a -> bool) context =
  let response = Option.get context.response in
  let parsed_body = Option.get response.parsed_body in

  Alcotest.(check bool) "Check the parsed body" true (f parsed_body);

  context

let then_db_should_have_n expected query_count context =
  let inner_db_count (module Db : DB) = Lwt_main.run @@ Db.find query_count () in
  let count = Result.get_ok @@ inner_db_count context.db in

  Alcotest.(check int) "Check the number of items by db_query" expected count;

  context

let then_db_should_not_have_any_items query_count response =
  then_db_should_have_n 0 query_count response

module Hack = struct
  let extract_item context =
    let response = Option.get context.response in
    let parsed_body = Option.get response.parsed_body in

    parsed_body
end
