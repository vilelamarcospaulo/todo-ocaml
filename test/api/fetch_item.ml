open Flow
open Create_item

let test_fetch_unexistend db =
  let request = Dream.request ~method_:`GET ~target:"/todos/0" "" in

  given_the_request request
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 404
  >!> then_the_body_should_be "Record not found"

let test_fetch_todo_by_id db =
  let expected_body = ref "" in

  create_todo ()
  >!> then_follow_a_new_request (fun ctx ->
          let created_item : Todo.Item.t = Flow.Hack.extract_item ctx in
          let url = "/todos/" ^ string_of_int created_item.id in

          expected_body := Todo.Item.yojson_of_t created_item |> Yojson.Safe.to_string;

          Dream.request ~method_:`GET ~target:url "")
  >!> with_db db
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> then_the_body_should_be_lazy expected_body

let suite =
  ( "[GET] /todo/{id}",
    [ ("fetch_unexistend", test_fetch_unexistend); ("fetch_todo_by_id", test_fetch_todo_by_id) ] )
