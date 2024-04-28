open Flow
open Create_item

let given_db_with_two_todos setup = setup >!> create_todo >!> create_todo

let list_all_todos_flow setup =
  setup
  >!> given_db_with_two_todos
  >!> given_the_request @@ Dream.request ~method_:`GET ~target:"/todos" ""
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body @@ Todo.Sql.Page.page_of_yojson Todo.Item.item_of_yojson
  >!> then_body_should_apply_to (fun (page : 'a Todo.Sql.Page.page) ->
          let check_item (item : Todo.Item.item) =
            Alcotest.(check string) "item.name" "foo" item.title;
            Alcotest.(check string) "item.description" "bar" item.description
          in
          List.iter check_item page.data;
          true)
  >!> then_body_should_apply_to (fun (page : 'a Todo.Sql.Page.page) ->
          Alcotest.(check int) "page.total_items" page.total_items 2;
          Alcotest.(check bool) "page.is_none" true (Option.is_none page.page);
          true)
  >! then_db_should_have_n 2 Todo.Item.Q.count_all

let suite = ("[GET] /todo", [ ("list_all_items_wihtout_page", list_all_todos_flow) ])
