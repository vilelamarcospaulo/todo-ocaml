open Flow

let create_item_invaid_payload_flow setup =
  let request = Dream.request ~method_:`POST ~target:"/todos" "{foo}" in

  setup
  >!> given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 400
  >!> then_the_body_should_be "Invalid request"
  >!> then_db_should_not_have_any_items Todo.Item.Q.count_all

let create_todo ?(to_create : Todo.Item.t_new_item = { title = "foo"; description = "bar" }) setup =
  let body = to_create |> Todo.Item.yojson_of_t_new_item |> Yojson.Safe.to_string in
  let request = Dream.request ~method_:`POST ~target:"/todos" body in

  setup
  >!> given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body Todo.Item.item_of_yojson
  >!> then_body_should_apply_to (fun (item : Todo.Item.item) ->
          item.title = to_create.title && item.description = to_create.description)

let create_todo_succesfully_flow setup =
  let expected_created_item =
    { Todo.Item.id = 1; title = "foo"; description = "bar"; completedAt = None }
  in
  let expected_body = expected_created_item |> Todo.Item.yojson_of_item |> Yojson.Safe.to_string in

  create_todo setup
  >!> then_the_body_should_be expected_body
  >!> then_body_should_apply_to (fun item -> item = expected_created_item)
  >!> then_db_should_have_n 1 Todo.Item.Q.count_all

let suite =
  ( "[POST] /todo",
    [
      ("invalid_payload", create_item_invaid_payload_flow);
      ("create_new_item", create_todo_succesfully_flow);
    ] )
