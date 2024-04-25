open Flow

let create_todo ?(to_create : Todo.Item.t_new_item = { title = "foo"; description = "bar" }) _ =
  let body = to_create |> Todo.Item.yojson_of_t_new_item |> Yojson.Safe.to_string in
  let request = Dream.request ~method_:`POST ~target:"/todos" body in

  given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body Todo.Item.t_of_yojson
  >!> then_body_should_apply_to (fun (item : Todo.Item.t) ->
          item.title = to_create.title && item.description = to_create.description)

let test_fetch_unexistend db =
  let request = Dream.request ~method_:`GET ~target:"/todos/0" "" in

  given_the_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 404
  >!> then_the_body_should_be "Record not found"

let test_fetch_todo_by_id db =
  let first_item = Flow.Hack.extract_item @@ create_todo db in

  let url = "/todos/" ^ string_of_int first_item.id in
  let request = Dream.request ~method_:`GET ~target:url "" in

  let expected_body = first_item |> Todo.Item.yojson_of_t |> Yojson.Safe.to_string in

  create_todo ()
  >!> then_given_a_new_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> then_the_body_should_be expected_body

let suite =
  ( "[GET] /todo/{id}",
    [ ("fetch_unexistend", test_fetch_unexistend); ("fetch_todo_by_id", test_fetch_todo_by_id) ] )
