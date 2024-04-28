open Flow
open Create_item

let test_fetch_unexistend setup =
  let request = Dream.request ~method_:`GET ~target:"/todos/0" "" in

  setup
  >!> given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 404
  >! then_the_body_should_be "Record not found"

let test_fetch_todo_by_id setup =
  let expected_body = ref "" in

  create_todo setup
  >!> then_follow_a_new_request (fun ctx ->
          let created_item : Todo.Item.item = Flow.Hack.extract_item ctx in
          let url = "/todos/" ^ string_of_int created_item.id in

          expected_body := Todo.Item.yojson_of_item created_item |> Yojson.Safe.to_string;

          Dream.request ~method_:`GET ~target:url "")
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >! then_the_body_should_be_lazy expected_body

let suite =
  ( "[GET] /todo/{id}",
    [ ("fetch_unexistend", test_fetch_unexistend); ("fetch_todo_by_id", test_fetch_todo_by_id) ] )
