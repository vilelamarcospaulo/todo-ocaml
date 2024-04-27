open Create_item
open Flow

let test_delete_unexistend setup =
  create_todo setup
  >!> then_follow_a_new_request (fun ctx ->
          let created_item : Todo.Item.item = Flow.Hack.extract_item ctx in
          let url = "/todos/" ^ string_of_int (created_item.id + 1) in

          Dream.request ~method_:`DELETE ~target:url "")
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 404
  >!> then_the_body_should_be "Record not found"

let test_delete_by_id setup =
  create_todo setup
  >!> then_follow_a_new_request (fun ctx ->
          let created_item : Todo.Item.item = Flow.Hack.extract_item ctx in
          let url = "/todos/" ^ string_of_int created_item.id in

          Dream.request ~method_:`DELETE ~target:url "")
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> then_db_should_not_have_any_items Todo.Item.Q.count_all

let suite =
  ( "[DELETE] /todo/{id}",
    [ ("delete_unexistend", test_delete_unexistend); ("delete_item_by_id", test_delete_by_id) ] )
