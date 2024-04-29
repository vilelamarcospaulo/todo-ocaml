open Flow
open Create_item

let given_db_with_todos n setup =
  let rec loop i state = if i = 0 then state else loop (i - 1) (state >!> create_todo) in

  loop n setup

let list_all_todos_flow setup =
  setup
  >!> given_db_with_todos 2
  >!> given_the_request @@ Dream.request ~method_:`GET ~target:"/todos" ""
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body @@ Todo.Sql.Page.page_of_yojson Todo.Item.item_of_yojson
  >!> then_body_should_apply_to (fun (query_resp : 'a Todo.Sql.Page.page) ->
          Alcotest.(check int) "reponse count" (List.length query_resp.data) 2;
          Alcotest.(check bool) "page.is_none" true (Option.is_none query_resp.page);
          true)
  >! then_body_should_apply_to (fun (query_reps : 'a Todo.Sql.Page.page) ->
         let check_item (item : Todo.Item.item) =
           Alcotest.(check string) "item.name" "foo" item.title;
           Alcotest.(check string) "item.description" "bar" item.description
         in
         List.iter check_item query_reps.data;
         true)

let list_paginated_todos_flow setup =
  let request = Dream.request ~method_:`GET ~target:"/todos?page=2&page_size=5" "" in

  setup
  >!> given_db_with_todos 10
  >!> given_the_request request
  >!> when_the_request_is_sent
  >!> then_the_status_should_be 200
  >!> after_parse_the_body @@ Todo.Sql.Page.page_of_yojson Todo.Item.item_of_yojson
  >!> then_body_should_apply_to (fun (query_resp : 'a Todo.Sql.Page.page) ->
          Alcotest.(check int) "reponse count" 5 (List.length query_resp.data);
          Alcotest.(check bool) "page.is_some" true (Option.is_some query_resp.page);
          true)
  >!> then_body_should_apply_to (fun (query_resp : 'a Todo.Sql.Page.page) ->
          let page = Option.get query_resp.page in

          Alcotest.(check int) "current_page should be" 2 page.page;
          Alcotest.(check int) "page_size should be " 5 page.size;
          Alcotest.(check int) "total_items should be " 10 page.total_items;
          Alcotest.(check int) "total_pages should be" 2 page.total_pages;
          true)
  >! then_body_should_apply_to (fun (query_resp : 'a Todo.Sql.Page.page) ->
         let first = List.hd query_resp.data in
         let last = List.hd @@ List.rev query_resp.data in

         Alcotest.(check int) "first item has id" 6 first.id;
         Alcotest.(check int) "last item has id" 10 last.id;
         true)

let suite =
  ( "[GET] /todo",
    [
      ("list_all_items_without_page", list_all_todos_flow);
      ("list_items_paginated", list_paginated_todos_flow);
    ] )
